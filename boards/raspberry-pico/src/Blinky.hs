module Blinky(main) where
import Prelude

defaultLed :: Int
defaultLed = 25

gpioLed :: Int
gpioLed = 20

gpioOut :: Int
gpioOut = 1

gpioIn :: Int
gpioIn = 0

main :: IO ()
main = do
  init
  blinky

init :: IO ()
init = do
  c_stdio_init_all
  c_gpio_init gpioLed
  c_gpio_init defaultLed
  c_gpio_set_dir gpioLed gpioOut
  c_gpio_set_dir defaultLed gpioOut

blinky :: IO ()
blinky = do
  setLed True
  wait 2000
  setLed False
  wait 2000
  blinky

foreign import ccall "set_led" set_led :: Int -> IO ()
foreign import ccall "delay_ms" delay_ms :: Int -> IO ()

foreign import ccall "pico/stdlib.h gpio_init"      c_gpio_init      :: Int -> IO ()
foreign import ccall "pico/stdlib.h gpio_set_dir"   c_gpio_set_dir   :: Int -> Int -> IO ()
foreign import ccall "pico/stdlib.h gpio_put"       c_gpio_put       :: Int -> Int -> IO ()
foreign import ccall "pico/stdlib.h gpio_get"       c_gpio_get       :: Int -> IO Int
foreign import ccall "pico/stdlib.h sleep_ms"       c_sleep_ms       :: Int -> IO ()
foreign import ccall "pico/stdlib.h stdio_init_all" c_stdio_init_all :: IO ()

setLed :: Bool -> IO ()
setLed on = do
  c_gpio_put defaultLed $ if on then 1 else 0
  c_gpio_put gpioLed $ if on then 1 else 0

wait :: Int -> IO ()
wait n = c_sleep_ms n
