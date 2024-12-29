cmake_minimum_required(VERSION 3.13)

# initialize the SDK based on PICO_SDK_PATH
# note: this must happen before project()
include(deps/pico-sdk/external/pico_sdk_import.cmake)

project(MicroHs_RP)

# initialize the Raspberry Pi Pico SDK
pico_sdk_init()

add_executable(blinky
    build/blinky.c
)

add_executable(blinky_btn
    build/blinky_btn.c
)

# pull in common dependencies
target_link_libraries(blinky pico_stdlib)
target_link_libraries(blinky_btn pico_stdlib)

pico_enable_stdio_usb(blinky 1)
pico_enable_stdio_uart(blinky 0)

pico_enable_stdio_usb(blinky_btn 1)
pico_enable_stdio_uart(blinky_btn 0)

# create map/bin/hex file etc.
pico_add_extra_outputs(blinky)
pico_add_extra_outputs(blinky_btn)
