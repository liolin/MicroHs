module Blinky(main) where
import Prelude

main :: IO ()
main = blinky (500::Int)

blinky :: Int -> IO ()
blinky n | n > 1000 = return ()
blinky n = do
  oneByOne n True
  oneByOne n False
  blinky (n + 1)

oneByOne :: Int -> Bool -> IO ()
oneByOne n on = forM_ [0..3] $ \ led -> do
  setLed on
  wait (n + led)

foreign import ccall "set_led" set_led :: Int -> IO ()
foreign import ccall "delay_ms" delay_ms :: Int -> IO ()


setLed :: Bool -> IO ()
setLed on = set_led $ if on then 1 else 0

wait :: Int -> IO ()
wait n = delay_ms (n*300)

loop :: Int -> ()
loop n = if n == 0 then () else loop (n - 1)


forM_ = flip mapM_

mapM_ f = foldr c (return ())
  where c x k = f x >> k
