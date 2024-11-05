cmake_minimum_required(VERSION 3.13)

# initialize the SDK based on PICO_SDK_PATH
# note: this must happen before project()
include(pico_sdk_import.cmake)

project(my_project)

# initialize the Raspberry Pi Pico SDK
pico_sdk_init()

add_executable(blink
    src/runtime/main.c
    )

# pull in common dependencies
target_link_libraries(blink pico_stdlib)

#if (PICO_CYW43_SUPPORTED)
#    target_link_libraries(blink pico_cyw43_arch_none)
#endif()

# create map/bin/hex file etc.
pico_add_extra_outputs(blink)
