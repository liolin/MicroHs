# Blinky for arduino uno

## How to build blinky
First build `mhs`, `mhseval` and `Addcombs` from the root directory.
Copy `Blinky.hs` to the MicroHs root dir and run:
```sh
bin/mhs Blinky -z -oblinky_compress.c
```

Copy `blinky_compress.c` to `src/runtime/`
Change directory to `boards/ArduinoUno/`
and run (does not work yet)
```sh
avr-gcc -mmcu=atmega328p -Os -o build/blinky.elf ./run.c
avr-objcopy -j .text -j .data -O ihex build/blinky.elf build/blinky.hex

```

And upload:
```sh
avrdude -c arduino -p m328p -P /dev/ttyACM0 -b 115200 -U flash:w:build/blink.hex
```
