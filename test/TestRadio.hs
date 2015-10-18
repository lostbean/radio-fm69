{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.State
import Data.Map (Map)
import Data.Word
import qualified Data.Map as M

import Radio.RFM69
import Radio.RFM69.Registers
import WiringHs

main :: IO ()
main = runRadio cfg $ do
  readRadioStatus
  let msg = map (fromIntegral . fromEnum) "Hello word!"
  s <- sendWithRetry (NodeAddress 122) msg Nothing Nothing
  liftIO $ putStrLn $ "Send: " ++ show s
  liftIO $ putStrLn "Done"

cfg :: Config
cfg = Config
  { freqBand    = F868MHZ
  , nodeId      = NodeAddress 0
  , netId       = NetworkId 1
  , broadcastId = BroadcastAddress 255
  , isRFM69HW   = False
  , intPin      = Pin 5
  , readAction  = liftIO . print
  }

-- ========================= Mock Monad ===================================

newtype Mock a = Mock { unMock :: State (Map Word8 Word8) a }
  deriving (Monad, Functor, MonadState (Map Word8 Word8))

instance SPI Mock where
  spiRW = error "Not implemented yet for Mock"
  spiSend a x = modify $ \m -> M.insert a x m
  spiGet a    = get >>= return . maybe 0 id . M.lookup a

runMock = flip execState defaultRegs . unMock

evalMock = flip evalState defaultRegs . unMock

defaultRegs :: Map Word8 Word8
defaultRegs = M.fromList
  [ (0x00, 0x00)
  , (0x01, 0x04)
  , (0x02, 0x00)
  , (0x03, 0x1a)
  , (0x04, 0x0b)
  , (0x05, 0x00)
  , (0x06, 0x52)
  , (0x07, 0xe4)
  , (0x08, 0xc0)
  , (0x09, 0x00)
  , (0x0a, 0x41)
  , (0x0b, 0x00)
  , (0x0d, 0x92)
  , (0x11, 0x9f)
  , (0x12, 0x09)
  , (0x23, 0x02)
  , (0x24, 0xff)
  , (0x27, 0x80)
  , (0x39, 0x00)

  ]
