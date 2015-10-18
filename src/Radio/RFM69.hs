{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Radio.RFM69 where
-- This module is inspired on https://github.com/LowPowerLab/RFM69

import Data.Maybe
import Data.Time
import Data.Word
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import qualified Data.Vector.Storable as V

import Radio.RFM69.Registers
import WiringHs as GPIO

newtype Radio a = Radio { unRadio :: ReaderT (Config, MVar RadioState) IO a }
  deriving (Monad, MonadIO, Functor, MonadReader (Config, MVar RadioState))

askCfg :: Radio Config
askCfg = fmap fst ask

asksCfg :: (Config -> a) -> Radio a
asksCfg f = fmap f askCfg

getState :: Radio RadioState
getState = ask >>= liftIO . readMVar . snd

setState :: RadioState -> Radio ()
setState st = ask >>= liftIO . flip swapMVar st . snd >> return ()

modifyState :: (RadioState -> RadioState) -> Radio ()
modifyState f = ask >>= liftIO . flip modifyMVar_ (return . f) . snd

data Band
  = F315MHZ
  | F433MHZ
  | F868MHZ
  | F915MHZ
  deriving (Show, Eq)

instance SPI Radio where
  spiRW = liftIO . fmap V.toList . wiringPiSPIDataRW Channel0 . V.fromList

newtype NetworkId = NetworkId {networkId :: Word8} deriving (Show, Eq, Ord)

instance Register NetworkId where
  regWrite (NetworkId a) = spiSend 0x30 . newRegister $ insertField a
  regRead = spiGet 0x30 >>= return . flip readRegister (liftM NetworkId readField)

data CtrlByte
  = CtrlByte
  { acknowledging :: Bool
  , requestACK    :: Bool
  } deriving (Show)

writeCtrlByte :: CtrlByte -> Word8
writeCtrlByte (CtrlByte a b) = newRegister $ do
  insertField a >> insertField b >> skipInsert 6

readCtrlByte :: Word8 -> CtrlByte
readCtrlByte r = readRegister r $ do
  skipRead 6
  b <- readField
  a <- readField
  return $ CtrlByte a b

data Config
  = Config
  { freqBand    :: Band
  , debugMode   :: Bool
  , nodeId      :: NodeAddress
  , netId       :: NetworkId
  , broadcastId :: BroadcastAddress
  , aesKey      :: Maybe AesKey
  , isRFM69HW   :: Bool
  , intPin      :: Pin
  , readAction  :: [Word8] -> Radio ()
  }

data RadioState
  = RadioState
  { intLock         :: Bool
  , mode            :: OpMode
  , promiscuousMode :: Bool
  , senderId        :: NodeAddress
  , targetId        :: NodeAddress
  , ackRequested    :: Bool
  , ackReceived     :: Bool
  , waitingAck      :: Maybe NodeAddress
  , rssi            :: RssiValue
  } deriving (Show)

initState :: RadioState
initState
  = RadioState
  { intLock         = False
  , mode            = Standby
  , promiscuousMode = False
  , senderId        = NodeAddress 0
  , targetId        = NodeAddress 0
  , ackRequested    = False
  , ackReceived     = False
  , waitingAck      = Nothing
  , rssi            = toEnum 0
  }

maxDataLen :: Int
maxDataLen = 60

csmaRssiLimit :: RssiValue
csmaRssiLimit = RssiValue (-90)

csmaTimeLimit :: NominalDiffTime
csmaTimeLimit = 1

runRadio :: Config -> Radio a -> IO a
runRadio cfg action = do
  -- initialize GPIO and SPI
  setOk <- wiringPiSetup Pins
  when (not setOk) $ error "Couldn't initialize the GPIO."
  pinMode (intPin cfg) Input
  wiringPiSPISetup Channel0 4000000

  newSt <- newMVar initState

  flip runReaderT (cfg, newSt) $ unRadio initRadio

  setInterrupt (intPin cfg) Rising (flip runReaderT (cfg, newSt) $ unRadio $ interruptHandler)

  flip runReaderT (cfg, newSt) $ unRadio action

initRadio :: Radio ()
initRadio = do
  cfg <- askCfg

  -- is SPI working ?
  waitFor "SPI check 1" ((== 0xaa) . syncValue1) (regWrite (SyncValue1 0xaa) >> regRead)
  waitFor "SPI check 2" ((== 0x55) . syncValue1) (regWrite (SyncValue1 0x55) >> regRead)

  regWrite
    $ RegOpMode
    { sequenceOff = False
    , listenOn    = False
    , listenAbort = False
    , opMode      = Standby
    }

  regWrite
    $ RegDataModul
    { dataMode   = Packet
    , modulType  = FSK
    , modulShape = NoShaping
    }

  regWrite $ BitRate 55555

  regWrite $ FreqDev 50000

  regWrite $ FreqRF $ case freqBand cfg of
    F315MHZ -> 315000000
    F433MHZ -> 433000000
    F868MHZ -> 868000000
    F915MHZ -> 915000000

  regWrite
    $ RegPaLevel
    { pa0On   = True
    , pa1On   = False
    , pa2On   = False
    , powerdB = OutputPower 13
    }

  --over current protection (default is 95mA)
  -- regWrite
  --   $ OCP
  --   { ocpOn   = True
  --   , ocpTrim = OCPTrim 95
  --   }

  regWrite
    $ RxBw
    { dccFreq  = DccFreq 2
    , rxBwMant = RxBwMant16
    , rxBwExp  = RxBwExp 2 -- when (BitRate < 2 * RxBw) then RxBwExp = 2
    }

  regWrite
    $ DIOMapping1
    { dIO0 = DIO_01
    , dIO1 = DIO_00
    , dIO2 = DIO_00
    , dIO3 = DIO_00
    }

  regWrite $ RssiThreshold (-110)

  regWrite
    $ SyncConfig
    { syncOn            = True
    , fifoFillCondition = OnSyncAddressIRQ
    , syncSize          = SyncSize 2
    , syncTolerance     = SyncTolerance 0
    }

  regWrite (SyncValue1 0x2d)

  regWrite (netId cfg)

  regWrite
    $ PacketConfig1
    { packetFormat     = VariableLength
    , dcFree           = NoDcFree
    , crcOn            = True
    , crcAutoClearOff  = False
    , addressFiltering = NoAddrFilter
    }

  regWrite $ PayloadLength 66

  regWrite $ PreambleSize 0x03

  -- address filtering not in use
  -- regWrite $ NodeAddress (nodeId cfg)

  regWrite
    $ FifoThresholdTx
    { startConditionTx = FifoNonEmpty
    , fifoThreshold    = FifoThreshold 0xff
    }

  regWrite
    $ PacketConfig2
    -- RXDelay must match transmitter PA ramp-down time (bitrate dependent)
    { interPacketRXDelay = RXDelay 1
    , restartRX          = False
    , autoRestartRX      = True
    , aes                = False
    }

  -- run DAGC continuously in RX mode, recommended default for AfcLowBetaOn=0
  regWrite AfcLowBetaOn0

  encrypt (aesKey cfg)
  setHighPower (isRFM69HW cfg)
  waitFor "init mode ready" isModeReady regRead

  rcCalibration

readRadioStatus :: Radio ()
readRadioStatus = do
  (regRead :: Radio RegOpMode    ) >>= liftIO . print
  (regRead :: Radio PacketConfig2) >>= liftIO . print
  (regRead :: Radio RegPaLevel   ) >>= liftIO . print
  (regRead :: Radio IRQFlags     ) >>= liftIO . print
  (regRead :: Radio IRQFlags2    ) >>= liftIO . print

dumpRegisters :: Radio ()
dumpRegisters = do
  forM_ [0x01 .. 0x4f] $ \addr -> do
    spiGet addr >>= liftIO . putStrLn . showReg addr

setMode :: OpMode -> Radio ()
setMode newMode = do
  -- unless (newMode == mode st) $ do
  regRead >>= \w -> regWrite $ w {opMode = newMode}
  modifyState $ \st -> st {mode = newMode}
  waitFor "set mode ready" isModeReady regRead
  ok <- regRead >>= return . (newMode ==) . opMode
  unless ok $ error "Can't set transceiver mode."
  when ok setPower
  where
    setPower = do
      cfg <- askCfg
      when (isRFM69HW cfg) $ case newMode of
        Transmitter -> setHighPowerRegs True
        Receiver    -> setHighPowerRegs False
        _           -> return ()

setHighPowerRegs :: Bool -> Radio ()
setHighPowerRegs on = regRead >>= \r ->
  if on then
    regWrite $ r {powerdB = OutputPower 13}
  else
    regWrite $ r {powerdB = OutputPower (-15)}

sleep :: Radio ()
sleep = setMode Sleep

setNetwork :: NetworkId -> Radio ()
setNetwork = regWrite

setPowerLevel :: OutputPower -> Radio ()
setPowerLevel p = regRead >>= \r -> regWrite $ r {powerdB = p}

canSend :: Radio Bool
canSend = do
  st <- getState
  case mode st of
    Standby -> return True
    Receiver -> do
      r <- readRSSI False
      -- detected if there is channel activity
      if r < csmaRssiLimit
        then setMode Standby >> return True
        else return False
    _ -> return False

send :: NodeAddress -> [Word8] -> Bool -> Radio ()
send toNodeAddress buff rACK = do
  regRead >>= \r -> regWrite $ r { restartRX = True }
  waitFor "low traffic" id $ do
    receiveDone >> liftIO (threadDelay 50000) >> canSend
  sendFrame toNodeAddress buff rACK False

sendWithRetry :: NodeAddress -> [Word8] -> Maybe Int -> Radio Bool
sendWithRetry toNodeAddress buff mRetries = do
  modifyState (\st -> st { waitingAck = Just toNodeAddress })
  sent <- tryToSend 0
  unless sent $ modifyState (\st -> st { waitingAck = Nothing })
  return sent
  where
    retries       = fromMaybe 3 mRetries
    checkTime     = 200000               -- 200ms between checks
    tryToSend i
      | i >= retries = return False
      | otherwise    = do
          send toNodeAddress buff True
          putStrLnD $ "Tx n." ++ show i
          ack <- waitACK 10
          if ack then return True else tryToSend (i + 1)
    -- add thread sleep
    waitACK k
      | k <= (0 :: Int) = return False
      | otherwise = do
          ok <- return . (Nothing ==) . waitingAck =<< getState
          if ok
            then return True
            else liftIO (threadDelay checkTime) >> waitACK (k - 1)

isAckRequested :: Radio Bool
isAckRequested = do
  st  <- getState
  cfg <- askCfg
  return $ ackRequested st && (nodeAddr $ targetId st) /= (broadcastAddr $ broadcastId cfg)

sendACK :: NodeAddress -> [Word8] -> Radio ()
sendACK toNodeAddress buff = do
  waitFor "can send ACK" id canSend
  sendFrame toNodeAddress buff False True

sendFrame :: NodeAddress -> [Word8] -> Bool -> Bool -> Radio ()
sendFrame target xs rACK sACK = do
  -- stop reception while filling buffer
  setMode Standby

  sender <- asksCfg nodeId
  let buff = take maxDataLen xs
      len  = fromIntegral $ length buff + 3
      ctrl = writeCtrlByte $ CtrlByte { requestACK = rACK, acknowledging = sACK }
  spiMultiSend 0x00 $ [len, nodeAddr target, nodeAddr sender, ctrl] ++ buff

  setMode Transmitter
  waitWhile "sendFrame: packet sent" id $ do
    liftIO (threadDelay 5000)
    liftM (\r -> not (packetSent r) && fifoNotEmpty r) regRead

  setMode Receiver

interruptHandler :: Radio ()
interruptHandler = do
  st  <- getState
  cfg <- askCfg
  hasPayload <- fmap payloadReady regRead
  when (mode st == Receiver && hasPayload) $ do
    modifyState $ \s -> s { intLock  = True }
    setMode Standby
    [len, target, sender, ctrl] <- spiMultiGet 0x0 4
    let senderNode = NodeAddress sender
    let targetNode = NodeAddress target
    let payLen = min maxDataLen (fromIntegral len)
    thisNode <- asksCfg nodeId
    if (not (promiscuousMode st || targetNode == thisNode || target == (broadcastAddr $ broadcastId cfg)))
      then setState $ st { intLock  = False }
      else do
        let dataLen  = payLen - 3
        let ctrlbyte = readCtrlByte ctrl
        bs <- spiMultiGet 0x0 (fromIntegral dataLen)
        rs <- readRSSI False
        setState $ st
          { senderId     = senderNode
          , targetId     = targetNode
          , ackRequested = requestACK ctrlbyte
          , ackReceived  = acknowledging ctrlbyte
          , rssi         = rs
          , intLock      = False
          }
        when (requestACK ctrlbyte) $ do
          let ackMsg = map (toEnum . fromEnum) "acking"
          sendFrame senderNode ackMsg False True
          putStrLnD $ ">> ACK Sent to " ++ show senderNode
        if (acknowledging ctrlbyte && maybe False (senderNode ==) (waitingAck st))
          then do
            modifyState $ \s -> s { waitingAck = Nothing }
            putStrLnD "ACK Received"
          else forkRadio (readAction cfg bs) >> return ()
    resetReceiver
  modifyState $ \s -> s { intLock  = False }

resetReceiver :: Radio ()
resetReceiver = do
  irq <- regRead
  when (payloadReady irq) $ do
    -- avoid RX deadlocks
    regRead >>= \r -> regWrite $ r { restartRX = True }
  -- set DIO0 to "PAYLOADREADY" in receive mode
  regWrite $ DIOMapping1 DIO_01 DIO_00 DIO_00 DIO_00
  setMode Receiver

receiveDone :: Radio Bool
receiveDone = do
  st <- getState
  case mode st of
     Standby  -> return True
     Receiver -> setMode Standby >> return True
     _        -> receiveBegin >> return False

forkRadio :: Radio () -> Radio ThreadId
forkRadio act = do
  radio <- ask
  liftIO . forkIO . flip runReaderT radio $ unRadio act

receiveBegin :: Radio ()
receiveBegin = do
  waitWhile "int locked" intLock getState
  modifyState $ \st -> st
    { senderId     = NodeAddress 0
    , targetId     = NodeAddress 0
    , ackRequested = False
    , ackReceived  = False
    , rssi         = toEnum 0
    }
  resetReceiver

readRSSI :: Bool -> Radio RssiValue
readRSSI forceTrigger = do
  when forceTrigger $ do
    regRead >>= \r -> regWrite $ r {rssiStart = True}
    waitFor "RSSI done" rssiDone regRead
  regRead

putStrLnD :: String -> Radio ()
putStrLnD s = asksCfg debugMode >>= \dbg -> when dbg (liftIO $ putStrLn s)

waitWhile :: String -> (a -> Bool) -> Radio a -> Radio ()
waitWhile tag test m = go
  where
    msg = "Waiting while " ++ tag
    go = m >>= \x -> when (test x) (putStrLnD msg >> go)

waitFor :: String -> (a -> Bool) -> Radio a -> Radio ()
waitFor tag test m = go
  where
    msg = "Waiting for " ++ tag
    go = m >>= \x -> unless (test x) (putStrLnD msg >> go)

newtype AesKey = AesKey [Word8]

mkAESKey :: String -> Maybe AesKey
mkAESKey xs
  | length xs == 16 = Just $ AesKey $ map (toEnum . fromEnum) xs
  | otherwise       = Nothing

encrypt :: Maybe AesKey -> Radio ()
encrypt mkey = do
  setMode Standby
  isAES <- case mkey of
    Just (AesKey key) -> spiMultiSend 0x3e (map fromIntegral key) >> return True
    _                 -> return False
  regRead >>= \r -> regWrite $ r {aes = isAES}

rcCalibration :: Radio ()
rcCalibration = do
  regWrite $ RegOsc1 { rcCalStart = True, rcCalDone = False }
  waitFor "rc calibration" rcCalDone regRead

setHighPower :: Bool -> Radio ()
setHighPower on
  | on = do
      regRead >>= \r -> regWrite $ r {ocpOn = False}
      -- enable P1 & P2 amplifier stages
      regRead >>= \r -> regWrite $ r {pa1On = True, pa2On = True}
  | otherwise = do
      regRead >>= \r -> regWrite $ r {ocpOn = True}
      -- enable P0 only
      regRead >>= \r -> regWrite $ r {pa0On = True, pa1On = False, pa2On = False, powerdB = OutputPower 13}

shutdown :: Radio ()
shutdown = do
  setHighPower False
  sleep
