cmake_minimum_required(VERSION 3.13)

# initialize the SDK based on PICO_SDK_PATH
# note: this must happen before project()
include(deps/pico-sdk/external/pico_sdk_import.cmake)

project(blinky)

# initialize the Raspberry Pi Pico SDK
pico_sdk_init()

add_executable(blink
    build/main.c
)

# pull in common dependencies
target_link_libraries(blink pico_stdlib)

pico_enable_stdio_usb(blink 1)
pico_enable_stdio_uart(blink 0)

# create map/bin/hex file etc.
pico_add_extra_outputs(blink)
