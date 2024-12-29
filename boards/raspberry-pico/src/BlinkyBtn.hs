module BlinkyBtn(main) where
import Prelude

defaultLed :: Int
defaultLed = 25

gpioLed :: Int
gpioLed = 20

gpioButton :: Int
gpioButton = 19


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
  c_gpio_init gpioLed
  c_gpio_init gpioButton
  c_gpio_set_dir gpioLed gpioOut
  c_gpio_set_dir gpioButton gpioIn

blinky :: IO ()
blinky = do
  showLed <- isButtonPressed
  if showLed
    then setExternalLed True
    else setDefaultLed True

  wait 250

  if showLed
    then setExternalLed False
    else setDefaultLed False

  wait 250
  blinky

foreign import ccall "set_led" set_led :: Int -> IO ()
foreign import ccall "delay_ms" delay_ms :: Int -> IO ()

foreign import ccall "pico/stdlib.h gpio_init"      c_gpio_init      :: Int -> IO ()
foreign import ccall "pico/stdlib.h gpio_set_dir"   c_gpio_set_dir   :: Int -> Int -> IO ()
foreign import ccall "pico/stdlib.h gpio_put"       c_gpio_put       :: Int -> Int -> IO ()
foreign import ccall "pico/stdlib.h gpio_get"       c_gpio_get       :: Int -> IO Int

foreign import ccall "pico/stdlib.h sleep_ms"       c_sleep_ms       :: Int -> IO ()

setExternalLed :: Bool -> IO ()
setExternalLed on = c_gpio_put gpioLed $ if on then 1 else 0

setDefaultLed :: Bool -> IO ()
setDefaultLed on = c_gpio_put defaultLed $ if on then 1 else 0

setLed :: Bool -> IO ()
setLed on = do
  c_gpio_put defaultLed $ if on then 1 else 0
  c_gpio_put gpioLed $ if on then 1 else 0

isButtonPressed :: IO Bool
isButtonPressed = do
  value <- c_gpio_get gpioButton
  pure $ if value == 1
    then True
    else False

wait :: Int -> IO ()
wait n = c_sleep_ms n
