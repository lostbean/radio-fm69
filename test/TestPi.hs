import WiringHs
import Control.Concurrent.Thread.Delay
import Control.Monad

pinI, pinO :: Pin
pinI = Pin 4
pinO = Pin 3

main :: IO ()
main = do
  set <- wiringPiSetup Pins
  when (not set) $ error "Couldn't set the GPIO."
  pinMode pinI Input
  pinMode pinO Output

  setInterrupt pinI Rising (putStrLn "I felt something here!")

  forM_ [1..10] $ \n -> do
    putStr ("pin " ++ show n ++ " is ")
    digitalRead (Pin n) >>= putStrLn . show

  forM_ [1..10] $ \i -> do
    putStrLn $ "Cycle " ++ show (i :: Int)
    digitalWrite pinO Low
    delay 950000
    digitalWrite pinO High
    delay 50000

  putStrLn "End"
  digitalWrite pinO Low
