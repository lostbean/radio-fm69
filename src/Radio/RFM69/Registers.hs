{-# LANGUAGE
    BangPatterns
  , ExistentialQuantification
  , GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  #-}

module Radio.RFM69.Registers where

import Control.Monad
import Data.Bits
import Data.Char (intToDigit)
import Data.Word
import Numeric (showHex, showIntAtBase)

import Control.Monad.State

fXoSc :: Double
fXoSc = 32e6

fStep :: Double
fStep = fXoSc / 524288

-- ========================= Reg Monad ===================================

newtype Reg w a = Reg { unReg :: State (BitShift, w) a }
  deriving (Monad, Functor, MonadState (BitShift, w))

-- ====================================================================

class Monad m => SPI m where
  spiRW :: [Word8] -> m [Word8]

  spiSend :: Word8 -> Word8 -> m ()
  spiSend addr w = spiMultiSend addr [w]

  spiGet :: Word8 -> m Word8
  spiGet addr = spiMultiGet addr 1 >>= \ws -> case ws of
    [w] -> return w
    _   -> error "Unexpected number of bytes read by SPI interface."

  spiMultiSend :: Word8 -> [Word8] -> m ()
  spiMultiSend addr ws = spiRW ((addr .|. 0x80) : ws) >> return ()

  spiMultiGet :: Word8 -> Int -> m [Word8]
  spiMultiGet addr n =  return . tail =<< spiRW ((addr .&. 0x7f) : (replicate n 0xff))

class Register a where
  regWrite :: (SPI m, Monad m, Functor m) => a -> m ()
  regRead  :: (SPI m, Monad m, Functor m) => m a

class BitLength a where
  bitLength :: (Enum a, Bounded a) => a -> Int
  bitLength _ = getLen (maxBound :: a)

newtype BitLen   = Len   Int deriving (Show, Eq, Num)
newtype BitShift = Shift Int deriving (Show, Eq, Num)

newRegister :: (Num w) => Reg w a -> w
newRegister = snd . flip execState (0, 0) . unReg

readRegister :: (Num w) => w -> Reg w a -> a
readRegister x = flip evalState (0, x) . unReg

insertField :: forall w a. (BitLength a, Bits w, Num w, Bounded a, Enum a) => a -> Reg w ()
insertField x = modify $ \(s, w) -> (s, (w `shiftL` len) .|. genMask x)
  where
    len     = bitLength (maxBound :: a)
    genMask = (.&. mkMask (Len len)) . fromIntegral . fromEnum

readField :: forall w a. (BitLength a, Bits w, Integral w, Num w, Bounded a, Enum a) => Reg w a
readField = do
  (s, w) <- get
  let x = toEnum $ fromIntegral (w .&. mkMask (Len len))
  put (s, w `shiftR` len) >> return x
  where
    len = bitLength (maxBound :: a)

skipInsert :: forall w. (Bits w) => Int -> Reg w ()
skipInsert len = modify $ \(s, w) -> (s, w `shiftL` len)

skipRead :: forall w. (Bits w) => Int -> Reg w ()
skipRead len = modify $ \(s, w) -> (s, w `shiftR` len)

mkMask :: (Num b, Bits b) => BitLen -> b
mkMask (Len len)
  | len <= 0  = 0
  | otherwise = (bit len) - 1

getLen :: Enum a => a -> Int
getLen x = go 1 (fromEnum x)
  where
    go !n !acc
      | q <= 0    = n
      | otherwise = go (n + 1) q
      where q = acc `shiftR` 1

showIOOI :: (Show a, Integral a) => a -> String
showIOOI x = replicate (8 - length b) '0' ++ b
  where b = showIntAtBase 2 intToDigit x ""

showReg :: Word8 -> Word8 -> String
showReg addr w = showIOOI w ++ " (0x" ++ showHex w "" ++ ")" ++ " @ 0x" ++ showHex addr ""

instance SPI IO where
  spiRW = error "Not implemented for IO"
  spiSend a = putStrLn . showReg a
  spiGet a  = putStr (show a ++ " | ") >> readLn

instance BitLength Bool where
  bitLength = const 1

instance BitLength Word8 where
  bitLength = const 8

-- ******************************************************
--  RF69/SX1231 bit control definition
-- ******************************************************

-- | FIFO

instance Register Word8 where
  regWrite = spiSend 0x00
  regRead  = spiGet 0x00

-- | Operational mode
data RegOpMode
  = RegOpMode
  { sequenceOff :: Bool
  , listenOn    :: Bool
  , listenAbort :: Bool
  , opMode      :: OpMode
  } deriving Show

instance Register RegOpMode where
  regWrite (RegOpMode a b c d) = spiSend 0x01 . newRegister $ insertField a >> insertField b >> insertField c >> insertField d >> skipInsert 2
  regRead =  spiGet 0x01 >>= \w -> return . readRegister w $ do
    skipRead 2
    d <- readField
    c <- readField
    b <- readField
    a <- readField
    return $ RegOpMode a b c d

data OpMode
  = Sleep
  | Standby
  | Synthesizer
  | Transmitter
  | Receiver
  deriving (Enum, Eq, Bounded, Show)

instance BitLength OpMode

-- =====================
-- | Data Modulation
data RegDataModul
  = RegDataModul
  { dataMode   :: DataMode
  , modulType  :: ModulationType
  , modulShape :: ModulationShaping
  } deriving Show

instance Register RegDataModul where
  regWrite (RegDataModul a b c) = spiSend 0x02 . newRegister $ skipInsert 1 >> insertField a >> insertField b >> skipInsert 1 >> insertField c
  regRead =  spiGet 0x02 >>= \w -> return . readRegister w $ do
    c <- readField
    skipRead 1
    b <- readField
    a <- readField
    skipRead 1
    return $ RegDataModul a b c

data DataMode
  = Packet
  | ReserverdDataMode
  | Continuous
  | ContinuousNoSync
  deriving (Enum, Eq, Bounded, Show)

instance BitLength DataMode

data ModulationType
  = FSK
  | OOK
  deriving (Enum, Eq, Bounded, Show)

instance BitLength ModulationType

data ModulationShaping
  = NoShaping
  | GaussianBT1
  | GaussianBT0_5
  | GaussianBT0_3
  deriving (Enum, Eq, Bounded, Show)

instance BitLength ModulationShaping

-- =====================
-- | RegBitRate (bits/sec) example bit rates
newtype BitRate = BitRate Int deriving Show

instance Enum BitRate where
  toEnum = BitRate . round . (fXoSc /) . fromIntegral
  fromEnum (BitRate b) = round $ fXoSc / fromIntegral b

instance Register BitRate where
  regWrite b = do
    spiSend 0x03 h
    spiSend 0x04 l
    where (l, h) = readRegister (fromEnum b) $ liftM2 (,) readField readField
  regRead = do
    h <- spiGet 0x03
    l <- spiGet 0x04
    return . toEnum . newRegister $ insertField h >> insertField l

-- =====================
-- | RegFdev
newtype FreqDev = FreqDev Int deriving Show

instance Enum FreqDev where
  toEnum f = FreqDev . round $ fStep * fromIntegral f
  fromEnum (FreqDev f) = round (fromIntegral f / fStep)

instance Register FreqDev where
  regWrite b = do
    spiSend 0x05 h
    spiSend 0x06 l
    where (l, h) = readRegister (fromEnum b) $ liftM2 (,) readField readField
  regRead = do
    h <- spiGet 0x05
    l <- spiGet 0x06
    return . toEnum . newRegister $ insertField h >> insertField l

--- =====================
-- | RegFr
newtype FreqRF = FreqRF Word32 deriving Show

instance Register FreqRF where
  regWrite b = do
    spiSend 0x07 h
    spiSend 0x08 m
    spiSend 0x09 l
    where (l, m, h) = readRegister (fromEnum b) $ liftM3 (,,) readField readField readField
  regRead = do
    h <- spiGet 0x07
    m <- spiGet 0x08
    l <- spiGet 0x09
    return . toEnum . newRegister $ insertField h >> insertField m >> insertField l

instance Enum FreqRF where
  toEnum f = FreqRF . round $ fStep * fromIntegral f
  fromEnum (FreqRF f) = round (fromIntegral f / fStep)

instance Bounded FreqRF where
  minBound = FreqRF 0
  maxBound = FreqRF 0x0fff

-- =====================
-- | RegOsc1
data RegOsc1 = RegOsc1 { rcCalStart :: Bool, rcCalDone :: Bool } deriving Show

instance Register RegOsc1 where
  regWrite (RegOsc1 a b) = spiSend 0x0a . newRegister $ insertField a >> insertField b >> skipInsert 6
  regRead = spiGet 0x0a >>= \w -> return . readRegister w $ do
    skipRead 6
    b <- readField
    a <- readField
    return $ RegOsc1 a b

-- =====================
-- | RegAfcCtrl
newtype RegAfcCtrl = RegAfcCtrl Bool deriving Show

instance Register RegAfcCtrl where
  regWrite (RegAfcCtrl b) = spiSend 0x0b . newRegister $ insertField b
  regRead = spiGet 0x0b >>= return . RegAfcCtrl . flip readRegister readField

-- =====================
-- RegListen1
data RegListen1
  = RegListen1
  { resolIdle      :: ListenResol
  , resolRX        :: ListenResol
  , listenCriteria :: ListenCriteria
  , listenEnd      :: ListenEnd
  } deriving Show

instance Register RegListen1 where
  regWrite (RegListen1 a b c d) = spiSend 0x0d . newRegister $
    insertField a >> insertField b >> insertField c >> insertField d >> skipInsert 1
  regRead = spiGet 0x0d >>= \w -> return . readRegister w $ do
    skipRead 1
    d <- readField
    c <- readField
    b <- readField
    a <- readField
    return $ RegListen1 a b c d

data ListenResol
  = ReseverdListenResol
  | Resol64us
  | Resol4100us
  | Resol262000us
  deriving (Enum, Eq, Bounded, Show)

instance BitLength ListenResol

data ListenCriteria
  = Rssi
  | RssiAnsSyns
  deriving (Enum, Eq, Bounded, Show)

instance BitLength ListenCriteria

data ListenEnd
  = StaysRX
  | UntilPayload
  | ListenAutoResume
  deriving (Enum, Eq, Bounded, Show)

instance BitLength ListenEnd

-- ================================== Transmission ==============================================

-- =====================
-- RegPaLevel
data RegPaLevel
  = RegPaLevel
  { pa0On   :: Bool
  , pa1On   :: Bool
  , pa2On   :: Bool
  , powerdB :: OutputPower
  } deriving Show

instance Register RegPaLevel where
  regWrite (RegPaLevel a b c d) = spiSend 0x11 . newRegister $ insertField a >> insertField b >> insertField c >> insertField d
  regRead = spiGet 0x11 >>= \w -> return . readRegister w $ do
    d <- readField
    c <- readField
    b <- readField
    a <- readField
    return $ RegPaLevel a b c d

newtype OutputPower = OutputPower Int deriving (Ord, Eq, Show)

instance Enum OutputPower where
  toEnum = OutputPower . (\x -> x - 18) . fromIntegral
  fromEnum (OutputPower x) = fromIntegral $ x + 18

instance Bounded OutputPower where
  minBound = OutputPower (-18)
  maxBound = OutputPower 13

instance BitLength OutputPower where
  bitLength = const 5

-- =====================
-- RegPaRamp
data RegPaRamp
  = Ramp3400
  | Ramp2000
  | Ramp1000
  | Ramp500
  | Ramp250
  | Ramp125
  | Ramp100
  | Ramp62
  | Ramp50
  | Ramp40
  | Ramp31
  | Ramp25
  | Ramp20
  | Ramp15
  | Ramp12
  | Ramp10
  deriving (Enum, Eq, Bounded, Show)

instance BitLength RegPaRamp

instance Register RegPaRamp where
  regWrite = spiSend 0x12 . newRegister . insertField
  regRead = spiGet 0x12 >>= return . flip readRegister readField

-- =====================
-- RegOCP
data OCP = OCP {ocpOn :: Bool, ocpTrim :: OCPTrim} deriving Show

instance Register OCP where
  regWrite (OCP a b) = spiSend 0x13 . newRegister $
    skipInsert 3 >> insertField a >> insertField b
  regRead = spiGet 0x13 >>= \w -> return . readRegister w $ do
    b <- readField
    a <- readField
    skipRead 3
    return $ OCP a b

newtype OCPTrim = OCPTrim Int deriving (Eq, Show, Ord)

instance Enum OCPTrim where
  toEnum = OCPTrim . (+ 45) . (* 5) . fromIntegral
  fromEnum (OCPTrim x) = fromIntegral $ (x - 45) `quot` 5

instance BitLength OCPTrim where
  bitLength = const 4

instance Bounded OCPTrim where
  minBound = OCPTrim 120
  maxBound = OCPTrim 45

-- =====================
-- RegRxBw
data RxBw
  = RxBw
  { dccFreq  :: DccFreq
  , rxBwMant :: RxBwMant
  , rxBwExp  :: RxBwExp
  } deriving Show

instance Register RxBw where
  regWrite (RxBw a b c) = spiSend 0x13 . newRegister $
    insertField a >> insertField b >> insertField c
  regRead = spiGet 0x13 >>= \w -> return . readRegister w $ do
    c <- readField
    b <- readField
    a <- readField
    return $ RxBw a b c

newtype DccFreq = DccFreq Int deriving (Eq, Enum, Show, Ord)

instance BitLength DccFreq where
  bitLength = const 3

instance Bounded DccFreq where
  minBound = DccFreq 0
  maxBound = DccFreq 8

data RxBwMant
  = RxBwMant16
  | RxBwMant20
  | RxBwMant24
  deriving (Enum, Eq, Bounded, Show)

instance BitLength RxBwMant

newtype RxBwExp = RxBwExp Int deriving (Eq, Enum, Show, Ord)

instance BitLength RxBwExp where
  bitLength = const 3

instance Bounded RxBwExp where
  minBound = RxBwExp 0
  maxBound = RxBwExp 8

-- =====================
-- RegRssiConfig
data RssiConfig = RssiConfig {rssiDone :: Bool, rssiStart :: Bool} deriving Show

instance Register RssiConfig where
  regWrite (RssiConfig a b) = spiSend 0x23 . newRegister $
    skipInsert 6 >> insertField a >> insertField b
  regRead = spiGet 0x23 >>= \w -> return . readRegister w $ do
    b <- readField
    a <- readField
    skipRead 6
    return $ RssiConfig a b

-- =====================
-- RegRssiValue
newtype RssiValue = RssiValue Int deriving (Eq, Show, Ord)

instance Register RssiValue where
  regWrite = spiSend 0x24 . newRegister . insertField
  regRead  = spiGet 0x24 >>= return . flip readRegister readField

instance Enum RssiValue where
  toEnum = RssiValue . negate . (`quot` 2) . fromIntegral
  fromEnum (RssiValue x) = fromIntegral . (* 2) $ negate x

instance BitLength RssiValue where
  bitLength = const 8

instance Bounded RssiValue where
  minBound = RssiValue (-127)
  maxBound = RssiValue 0

-- =====================
-- RegIRQFlags1
data IRQFlags
  = IRQFlags
  { isModeReady        :: Bool
  , isRXReady          :: Bool
  , isTXReady          :: Bool
  , isPLLLock          :: Bool
  , isRSSI             :: Bool
  , isTimeout          :: Bool
  , isAutoMode         :: Bool
  , isSyncAddressMatch :: Bool
  } deriving (Eq, Show)

instance Register IRQFlags where
  regWrite (IRQFlags a b c d e f g h) = spiSend 0x27 . newRegister $
    insertField a >> insertField b >> insertField c >> insertField d >>
    insertField e >> insertField f >> insertField g >> insertField h
  regRead = spiGet 0x27 >>= \w -> return . readRegister w $ do
    h <- readField
    g <- readField
    f <- readField
    e <- readField
    d <- readField
    c <- readField
    b <- readField
    a <- readField
    return $ IRQFlags a b c d e f g h

-- =====================
-- RegIrqFlags2
data IRQFlags2 = IRQFlags2
  { fifoFull     :: Bool
  , fifoNotEmpty :: Bool
  , fifoLevel    :: Bool
  , fifoOverrun  :: Bool
  , packetSent   :: Bool
  , payloadReady :: Bool
  , crcOk        :: Bool
  , lowbat       :: Bool
  } deriving (Eq, Show)

instance Register IRQFlags2 where
  regWrite (IRQFlags2 a b c d e f g h) = spiSend 0x28 . newRegister $ do
    insertField a >> insertField b >> insertField c >> insertField d
    insertField e >> insertField f >> insertField g >> insertField h
  regRead = spiGet 0x28 >>= \r -> return $ readRegister r $ do
    h <- readField
    g <- readField
    f <- readField
    e <- readField
    d <- readField
    c <- readField
    b <- readField
    a <- readField
    return $ IRQFlags2 a b c d e f g h

-- =====================
-- RegRssiThreshold
newtype RssiThreshold = RssiThreshold Int deriving (Eq, Show, Ord)

instance Register RssiThreshold where
  regWrite = spiSend 0x29 . newRegister . insertField
  regRead  = spiGet 0x29 >>= return . flip readRegister readField

instance Enum RssiThreshold where
  toEnum = RssiThreshold . negate . (`quot` 2) . fromIntegral
  fromEnum (RssiThreshold x) = fromIntegral . (* 2) $ negate x

instance BitLength RssiThreshold where
  bitLength = const 8

instance Bounded RssiThreshold where
  minBound = RssiThreshold (-127)
  maxBound = RssiThreshold 0

--- =====================
-- | RegPreamble
newtype PreambleSize = PreambleSize Word32 deriving (Enum, Show)

instance Register PreambleSize where
  regWrite b = do
    spiSend 0x2c h
    spiSend 0x2d l
    where (l, h) = readRegister (fromEnum b) $ liftM2 (,) readField readField
  regRead = do
    h <- spiGet 0x2c
    l <- spiGet 0x2d
    return . toEnum . newRegister $ insertField h >> insertField l

instance Bounded PreambleSize where
  minBound = PreambleSize 0
  maxBound = PreambleSize 0x00ff

-- =====================
-- RegSyncConfig
data SyncConfig = SyncConfig
  { syncOn            :: Bool
  , fifoFillCondition :: FifoFillCondition
  , syncSize          :: SyncSize
  , syncTolerance     :: SyncTolerance
  } deriving Show

instance Register SyncConfig where
  regWrite (SyncConfig a b c d) = spiSend 0x2e . newRegister $ do
    insertField a >> insertField b >> insertField c >> insertField d
  regRead = spiGet 0x2e >>= \r -> return $ readRegister r $ do
    d <- readField
    c <- readField
    b <- readField
    a <- readField
    return $ SyncConfig a b c d

data FifoFillCondition
  = OnSyncAddressIRQ
  | AutoFifoFilling
  deriving (Enum, Eq, Bounded, Show)

instance BitLength FifoFillCondition where
  bitLength = const 1

newtype SyncSize = SyncSize Word8 deriving (Eq, Show)

instance BitLength SyncSize where
  bitLength = const 3

instance Enum SyncSize where
  toEnum = SyncSize . (+ 1) . fromIntegral
  fromEnum (SyncSize x) = fromIntegral $ x - 1

instance Bounded SyncSize where
  minBound = SyncSize 1
  maxBound = SyncSize 8

newtype SyncTolerance = SyncTolerance Word8 deriving (Enum, Eq, Bounded, Show)

instance BitLength SyncTolerance where
  bitLength = const 3

-- =====================
-- RegSyncValue1
newtype SyncValue1 = SyncValue1 { syncValue1 :: Word8 } deriving (Enum, Eq, Bounded, Show)

instance BitLength SyncValue1 where
  bitLength = const 8

instance Register SyncValue1 where
  regWrite = spiSend 0x2f . newRegister . insertField
  regRead  = spiGet 0x2f >>= return . flip readRegister readField

-- =====================
-- RegPacketConfig1
data PacketConfig1 = PacketConfig1
  { packetFormat     :: PacketFormat
  , dcFree           :: DcFree
  , crcOn            :: Bool
  , crcAutoClearOff  :: Bool
  , addressFiltering :: AddrFilter
  } deriving Show

instance Register PacketConfig1 where
  regWrite (PacketConfig1 a b c d e) = spiSend 0x37 . newRegister $ do
    insertField a >> insertField b >> insertField c >> insertField d >> insertField e >> skipInsert 1
  regRead = spiGet 0x37 >>= \r -> return $ readRegister r $ do
    skipRead 1
    e <- readField
    d <- readField
    c <- readField
    b <- readField
    a <- readField
    return $ PacketConfig1 a b c d e

data DcFree
  = NoDcFree
  | Manchester
  | Whitening
  deriving (Enum, Eq, Bounded, Show)

instance BitLength DcFree where
  bitLength = const 2

data PacketFormat
  = FixedLength
  | VariableLength
  deriving (Enum, Eq, Bounded, Show)

instance BitLength PacketFormat where
  bitLength = const 1

data AddrFilter
  = NoAddrFilter
  | MatchNodeAddress
  | MatchNodeAndBroadcastAddress
  deriving (Enum, Eq, Bounded, Show)

instance BitLength AddrFilter where
  bitLength = const 2

-- =====================
-- RegPayloadLength
newtype PayloadLength = PayloadLength { payloadLength :: Word8 } deriving (Enum, Eq, Bounded, Show)

instance BitLength PayloadLength where
  bitLength = const 8

instance Register PayloadLength where
  regWrite = spiSend 0x38 . newRegister . insertField
  regRead  = spiGet 0x38 >>= return . flip readRegister readField

-- =====================
-- RegNodeAddress
newtype NodeAddress = NodeAddress { nodeAddr :: Word8 } deriving (Enum, Eq, Bounded, Show)

instance BitLength NodeAddress where
  bitLength = const 8

instance Register NodeAddress where
  regWrite = spiSend 0x39 . newRegister . insertField
  regRead  = spiGet 0x39 >>= return . flip readRegister readField

-- =====================
-- RegBroadcastAddress
newtype BroadcastAddress = BroadcastAddress { broadcastAddr :: Word8 } deriving (Enum, Eq, Bounded, Show)

instance BitLength BroadcastAddress where
  bitLength = const 8

instance Register BroadcastAddress where
  regWrite = spiSend 0x3a . newRegister . insertField
  regRead  = spiGet 0x3a >>= return . flip readRegister readField

-- =====================
-- RegFifoThres
data FifoThresholdTx = FifoThresholdTx
  { startConditionTx :: StartConditionTx
  , fifoThreshold    :: FifoThreshold
  } deriving Show

instance Register FifoThresholdTx where
  regWrite (FifoThresholdTx a b) = spiSend 0x3c . newRegister $ insertField a >> insertField b
  regRead = spiGet 0x3c >>= \r -> return $ readRegister r $ do
    b <- readField
    a <- readField
    return $ FifoThresholdTx a b

data StartConditionTx
  = FifoLevel
  | FifoNonEmpty
  deriving (Enum, Eq, Bounded, Show)

instance BitLength StartConditionTx

newtype FifoThreshold = FifoThreshold { fifoThresLevel :: Word8 } deriving (Enum, Eq, Bounded, Show)

instance BitLength FifoThreshold where
  bitLength = const 7

-- =====================
-- RegPacketConfig2
data PacketConfig2 = PacketConfig2
  { interPacketRXDelay :: RXDelay
  , restartRX          :: Bool
  , autoRestartRX      :: Bool
  , aes                :: Bool
  } deriving Show

instance Register PacketConfig2 where
  regWrite (PacketConfig2 a b c d) = spiSend 0x3d . newRegister $ do
    insertField a >> skipInsert 1 >> insertField b >> insertField c >> insertField d
  regRead = spiGet 0x3d >>= \r -> return $ readRegister r $ do
    d <- readField
    c <- readField
    b <- readField
    skipRead 1
    a <- readField
    return $ PacketConfig2 a b c d

newtype RXDelay = RXDelay Word8 deriving Show

instance Enum RXDelay where
  toEnum = RXDelay . fromIntegral
  fromEnum (RXDelay x) = fromIntegral x

instance Bounded RXDelay where
  minBound = RXDelay 0x0
  maxBound = RXDelay 0xf

instance BitLength RXDelay where
  bitLength = const 4

-- =====================
-- RegDioMapping1
data DIOMapping1 = DIOMapping1
  { dIO0 :: DIOState
  , dIO1 :: DIOState
  , dIO2 :: DIOState
  , dIO3 :: DIOState
  }

instance Register DIOMapping1 where
  regWrite (DIOMapping1 a b c d) = spiSend 0x25 . newRegister $ do
    insertField a >> insertField b >> insertField c >> insertField d
  regRead = spiGet 0x25 >>= \r -> return $ readRegister r $ do
    d <- readField
    c <- readField
    b <- readField
    a <- readField
    return $ DIOMapping1 a b c d

data DIOState
  = DIO_00
  | DIO_01
  | DIO_10
  | DIO_11
  deriving (Enum, Eq, Bounded, Show)

instance BitLength DIOState where
  bitLength = const 2

-- =====================
-- RegDAGC
data DAGC
  = NoDAGC
  | AfcLowBetaOn1
  | AfcLowBetaOn0
  deriving (Eq, Bounded, Show)

instance Enum DAGC where
  toEnum x
    | x == 0x20 = AfcLowBetaOn1
    | x == 0x30 = AfcLowBetaOn0
    | otherwise = NoDAGC
  fromEnum x = case x of
    NoDAGC        -> 0x0
    AfcLowBetaOn1 -> 0x20
    AfcLowBetaOn0 -> 0x30

instance BitLength DAGC where
  bitLength = const 8

instance Register DAGC where
  regWrite = spiSend 0x6f . newRegister . insertField
  regRead  = spiGet 0x6f >>= return . flip readRegister readField
