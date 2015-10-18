{-# LANGUAGE
    LambdaCase
  , OverloadedStrings
  , ScopedTypeVariables
  , TupleSections
  #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Time
import Data.Word
import Network.Mail.Mime
import System.Posix.Syslog
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL

import Radio.RFM69
import Radio.RFM69.Registers
import WiringHs

type SendQueue = TQueue (NodeAddress, [Word8])

main :: IO ()
main = do
  queue <- atomically newTQueue
  let cfg = Config
          { freqBand    = F868MHZ
          , debugMode   = False
          , nodeId      = NodeAddress 1
          , netId       = NetworkId 118
          , broadcastId = BroadcastAddress 255
          , aesKey      = Just $ fromMaybe (error "Invalid AES key") $ mkAESKey "sampleEncryptKey"
          , isRFM69HW   = False
          , intPin      = Pin 5
          , readAction  = \ws -> logRx ws >> action queue ws
          }
  radioHub cfg queue

radioHub :: Config -> SendQueue -> IO ()
radioHub cfg queue = runRadio cfg $ do
  modifyState (\st -> st { promiscuousMode = True })
  when (debugMode cfg) dumpRegisters
  liftIO $ syslog Info "Radio is ready!"
  receiveBegin
  pinging queue [NodeAddress 2, NodeAddress 3, NodeAddress 4]
  loop
  where
    loop = do
      nextMsg <- liftIO $ atomically $ tryReadTQueue queue
      case nextMsg of
        Just (node, msg) -> do
          sent <- sendWithRetry node msg (Just 3)
          when (not sent) . liftIO $ do
            syslog Error $ "Couldn't send the message '" ++ show (map w2c msg) ++ "' to the " ++ show node
        Nothing -> return ()
      liftIO $ threadDelay 500000
      loop

pinging :: SendQueue -> [NodeAddress] -> Radio ()
pinging queue nodes = forkRadio loop >> return ()
  where
    loop = do
      liftIO $ do
        threadDelay 60000000
        forM_ nodes $ \n -> atomically $ writeTQueue queue (n, map c2w "ping")
      loop

logRx :: [Word8] -> Radio ()
logRx msg = do
  st <- getState
  let
    node = nodeAddr $ senderId st
    txt  = map (toEnum . fromEnum) msg
  liftIO $ syslog Info $ "(" ++ show node ++ ") -> " ++ txt ++ " | rssi: " ++ show (rssi st)

action :: SendQueue -> [Word8] -> Radio ()
action queue ws = case ws of
  [a]
    | a == c2w 'M' -> email "Motion" "Motion detected."
  [a, b]
    | a == c2w 'A' && b == 1 -> do
        email "Alarm" "Alarm is on."
        liftIO $ atomically $ writeTQueue queue (NodeAddress 4, map c2w "strobo")
    | a == c2w 'A' && b == 0 -> email "Alarm" "Alarm is off."
  _ -> email "Some Action" ("Unknown action: " ++ show ws)
  where
    email h b = do
      st  <- getState
      now <- liftIO getCurrentTime
      let
        header = T.pack $ h ++ " on node " ++ show (senderId st)
        body   = TL.pack $ b ++ "\n sent at " ++ showTime now
      let mail = simpleMail'
                 (Address (Just "Boat McBoaty") ("me@email.com"))
                 (Address (Just "Office Jonh" ) ("alert@email.com"))
                 header
                 body
      liftIO $ do
        syslog Alert $ "Sending email reporting: " ++ b
        renderSendMail mail

w2c :: Word8 -> Char
w2c = toEnum . fromIntegral

c2w :: Char -> Word8
c2w = fromIntegral . fromEnum

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%c"
