module Blinky(main) where
import Prelude

main :: IO ()
main = blinky

blinky :: IO ()
blinky = do
  setLed True
  wait 2000
  setLed False
  wait 2000
  blinky

foreign import ccall "set_led" set_led :: Int -> IO ()
foreign import ccall "delay_ms" delay_ms :: Int -> IO ()


setLed :: Bool -> IO ()
setLed on = set_led $ if on then 1 else 0

wait :: Int -> IO ()
wait n = delay_ms (n)
