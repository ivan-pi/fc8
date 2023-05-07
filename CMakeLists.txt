cmake_minimum_required(VERSION 3.15)

project(FC8
        VERSION 0.1.0
        DESCRIPTION "Fortran CHIP-8 interpreter"
        HOMEPAGE_URL "https://github.com/ivan-pi/FC8"
        LANGUAGES C Fortran)

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)
set(CMAKE_C_STANDARD 99)

find_package(X11 REQUIRED)

set(KEYBOARD "QWERTY" CACHE STRING "Keyboard layout; only options QWERTY and AZERTY are valid")
message(STATUS "Keyboard layout: ${KEYBOARD}")


add_executable(FC8-Xlib
    src/FC8-Xlib.f90 
    src/FC8_vm-Xlib.f90
    src/fcw-Xlib.f90
    src/fcw_Xlib.c
    src/randint8.c
    src/sleep.c
    src/sleep_std.f90
    )
target_link_libraries(FC8-Xlib "${X11_LIBRARIES}")
target_include_directories(FC8-Xlib PRIVATE "${X11_INCLUDE_DIR}")
if (KEYBOARD STREQUAL "AZERTY")
    target_compile_definitions(FC8-Xlib PUBLIC -DAZERTY)
endif()
