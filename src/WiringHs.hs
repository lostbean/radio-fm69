{-# LANGUAGE ForeignFunctionInterface #-}
module WiringHs where

import Data.Vector.Storable (Vector)
import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Storable as SV

-- This module is roughly based on https://github.com/bitterharvest/wiringPi

newtype Pin = Pin Int deriving Show

data Channel
  = Channel0
  | Channel1
  deriving (Show, Enum, Eq, Ord)

data WiringPiMode
  = Pins
  | Gpio
  | Sys
  | Phys
  deriving (Show, Enum, Eq, Ord)

data PinMode
  = Input
  | Output
  | PwmOutput
  | GpioClock
  | SoftPwmOutput
  | SoftToneOutput
  | PwmToneOutput
  deriving (Show, Enum, Eq, Ord)

data PinState
  = Low
  | High
  deriving (Show, Enum, Eq, Ord)

data PullMode
  = PullOff
  | PullDown
  | PullUp
  deriving (Show, Enum, Eq, Ord)

data PwmMode
  = PwmMS
  | PwmBal
  deriving (Show, Enum, Eq, Ord)

data IntEdge
  = Setup
  | Falling
  | Rising
  | Both
  deriving (Show, Enum, Eq, Ord)

data PiModel
  = ModelUnknown
  | ModelA
  | ModelB
  | ModelBP
  | ModelCM
  | ModelAP
  | Model2
  deriving (Show, Enum, Eq, Ord)

data PiVersion
  = VersionUnknown
  | Version1
  | Version1_1
  | Version1_2
  | Version2
  deriving (Show, Enum, Eq, Ord)

data PiMaker
  = MakerUnknown
  | Egoman
  | Sony
  | Qisda
  | Mbest
  deriving (Show, Enum, Eq, Ord)

foreign import ccall "wiring.h wiringPiSetup"     c_wiringPiSetup     :: IO CInt
foreign import ccall "wiring.h wiringPiSetupSys"  c_wiringPiSetupSys  :: IO CInt
foreign import ccall "wiring.h wiringPiSetupGpio" c_wiringPiSetupGpio :: IO CInt
foreign import ccall "wiring.h wiringPiSetupPhys" c_wiringPiSetupPhys :: IO CInt
wiringPiSetup :: WiringPiMode -> IO Bool
wiringPiSetup mode = fmap (>= 0) $ case mode of
  Pins -> c_wiringPiSetup
  Sys  -> c_wiringPiSetupSys
  Gpio -> c_wiringPiSetupGpio
  Phys -> c_wiringPiSetupPhys

foreign import ccall "wiring.h pinModeAlt" c_pinModeAlt :: CInt -> CInt -> IO ()
pinModeAlt :: Int -> Int -> IO ()
pinModeAlt pin mode = c_pinModeAlt (fromIntegral pin) (fromIntegral mode)

foreign import ccall "wiring.h pinMode" c_pinMode :: CInt -> CInt -> IO ()
pinMode :: Pin -> PinMode -> IO ()
pinMode (Pin pin) mode = c_pinMode (fromIntegral pin) (fromIntegral $ fromEnum mode)

foreign import ccall "wiring.h pullUpDnControl " c_pullUpDnControl :: CInt -> CInt -> IO ()
pullUpDnControl :: Pin -> PullMode -> IO ()
pullUpDnControl (Pin pin) pud =
  c_pullUpDnControl (fromIntegral pin) (fromIntegral $ fromEnum pud)

foreign import ccall "wiring.h digitalRead" c_digitalRead :: CInt -> IO CInt
digitalRead :: Pin -> IO PinState
digitalRead (Pin pin) = fmap (toEnum . fromIntegral) $ c_digitalRead (fromIntegral pin)

foreign import ccall "wiring.h digitalWrite" c_digitalWrite :: CInt -> CInt -> IO ()
digitalWrite :: Pin -> PinState -> IO ()
digitalWrite (Pin pin) value = c_digitalWrite (fromIntegral pin) (fromIntegral $ fromEnum value)

foreign import ccall "wiring.h pwmWrite" c_pwmWrite :: CInt -> CInt -> IO ()
pwmWrite :: Pin -> Word8 -> IO ()
pwmWrite (Pin pin) value = c_pwmWrite (fromIntegral pin) (fromIntegral value)

foreign import ccall "wiring.h analogRead" c_analogRead :: CInt -> IO CInt
analogRead :: Pin -> IO Word8
analogRead (Pin pin) = fmap fromIntegral $ c_analogRead (fromIntegral pin)

foreign import ccall "wiring.h analogWrite" c_analogWrite :: CInt -> CInt -> IO ()
analogWrite :: Pin -> Word8 -> IO ()
analogWrite (Pin pin) value = c_analogWrite (fromIntegral pin) (fromIntegral value)

foreign import ccall "wiring.h piBoardRev" c_piBoardRev :: IO CInt
piBoardRev :: IO Int
piBoardRev = fmap fromIntegral $ c_piBoardRev

foreign import ccall "wiring.h wpiPinToGpio" c_wpiPinToGpio :: CInt -> IO CInt
wpiPinToGpio :: Int -> IO Int
wpiPinToGpio wpiPin = fmap fromIntegral $ c_wpiPinToGpio (fromIntegral wpiPin)

foreign import ccall "wiring.h physPinToGpio" c_physPinToGpio :: CInt -> IO CInt
physPinToGpio :: Int -> IO Int
physPinToGpio physPin = fmap fromIntegral $ c_physPinToGpio (fromIntegral physPin)

foreign import ccall "wiring.h setPadDrive" c_setPadDrive :: CInt -> CInt -> IO ()
setPadDrive :: Int -> Int -> IO ()
setPadDrive group value = c_setPadDrive (fromIntegral group) (fromIntegral value)

foreign import ccall "wiring.h getAlt" c_getAlt :: CInt -> IO CInt
getAlt :: Int -> IO Int
getAlt pin = fmap fromIntegral $ c_getAlt (fromIntegral pin)

foreign import ccall "wiring.h pwmToneWrite" c_pwmToneWrite :: CInt -> CInt -> IO ()
pwmToneWrite :: Int -> Int -> IO ()
pwmToneWrite pin freq = c_pwmToneWrite (fromIntegral pin) (fromIntegral freq)

foreign import ccall "wiring.h digitalWriteByte" c_digitalWriteByte :: CInt -> IO ()
digitalWriteByte :: Int -> IO ()
digitalWriteByte value = c_digitalWriteByte (fromIntegral value)

foreign import ccall "wiring.h pwmSetMode" c_pwmSetMode :: CInt -> IO ()
pwmSetMode :: Int -> IO ()
pwmSetMode mode = c_pwmSetMode (fromIntegral mode)

foreign import ccall "wiring.h pwmSetRange" c_pwmSetRange :: CUInt -> IO ()
pwmSetRange :: Int -> IO ()
pwmSetRange range = c_pwmSetRange (fromIntegral range)

foreign import ccall "wiring.h pwmSetClock" c_pwmSetClock :: CInt -> IO ()
pwmSetClock :: Int -> IO ()
pwmSetClock divisor = c_pwmSetClock (fromIntegral divisor)

foreign import ccall "wiring.h gpioClockSet" c_gpioClockSet :: CInt -> CInt -> IO ()
gpioClockSet :: Int -> Int -> IO ()
gpioClockSet pin freq = c_gpioClockSet (fromIntegral pin) (fromIntegral freq)

-- ===================== SPI interface ====================

foreign import ccall "wiring.h wiringPiSPISetup"  c_wiringPiSPISetup  :: CInt -> CInt -> IO ()
foreign import ccall "wiring.h wiringPiSPIDataRW" c_wiringPiSPIDataRW :: CInt -> Ptr CUChar -> CInt -> IO ()

wiringPiSPISetup :: Channel -> Int -> IO ()
wiringPiSPISetup channel speed =
  c_wiringPiSPISetup (fromIntegral $ fromEnum channel) (fromIntegral speed)

wiringPiSPIDataRW :: Channel -> Vector Word8 -> IO (Vector Word8)
wiringPiSPIDataRW channel vs = do
  mv <- SV.unsafeFreeze =<< SV.thaw vs
  let len = SV.length vs
  SV.unsafeWith mv $ \ptr -> do
    c_wiringPiSPIDataRW (fromIntegral $ fromEnum channel) (castPtr ptr) (fromIntegral len)
    return mv

-- ===================== Interrupts ====================

foreign import ccall "wrapper" wrapper :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "wiring.h wiringPiISR" c_wiringPiISR :: Int -> Int -> FunPtr (IO ()) -> IO ()

setInterrupt :: Pin -> IntEdge -> IO () -> IO ()
setInterrupt (Pin p) mode callback = do
  func <- wrapper callback
  c_wiringPiISR (fromIntegral p) (fromIntegral $ fromEnum mode) func
