cmake_minimum_required(VERSION 3.16)

project(fc8
        VERSION 0.1.0
        DESCRIPTION "Fortran CHIP-8 interpreter"
        HOMEPAGE_URL "https://github.com/ivan-pi/FC8"
        LANGUAGES Fortran C)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING 
            "Build type (default = RelWithDebInfo)" FORCE)
endif()

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

#
# Public user settings
#
set(FC8_GRAPHICS x11 CACHE STRING "The graphics backend (default: x11)")
set_property(CACHE FC8_GRAPHICS PROPERTY STRINGS x11 sdl2)

set(FC8_KEYBOARD qwerty CACHE STRING "Keyboard layout (default: qwerty)")
set_property(CACHE FC8_KEYBOARD PROPERTY STRINGS qwerty qwertz azerty)

message(STATUS "Graphics: ${FC8_GRAPHICS}")
message(STATUS "Keyboard: ${FC8_KEYBOARD}")

#
# Internal project settings
#
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)
set(CMAKE_C_STANDARD 99)
set(BUILD_SHARED_LIBS false)

#
# Find graphics library
#
string(TOLOWER ${FC8_GRAPHICS} graphics)
if(graphics STREQUAL sfml)
    
    message(FATAL_ERROR "The sfml graphics backend is broken; use x11 instead.")

    set(SFML_BUILD_AUDIO false)
    set(SFML_BUILD_NETWORK false)
    set(SFML_INSTALL_PKGCONFIG_FILES false)

    enable_language(CXX)

    include(FetchContent)
    FetchContent_Declare(SFML
        GIT_REPOSITORY https://github.com/SFML/SFML.git
        GIT_TAG 2.6.x)
    FetchContent_MakeAvailable(SFML)

elseif(graphics STREQUAL sdl2)
    find_package(SDL2 REQUIRED CONFIG COMPONENTS SDL2)
else()
    find_package(X11 REQUIRED)
endif()

#
# I/O Library (graphics and keyboard)
#
set(C_OR_CPP $<IF:$<STREQUAL:${graphics},sfml>,cpp,c>)
add_library(fc8_io
    src/fc8_io.f90
    src/${graphics}/fc8_io.${C_OR_CPP})

target_include_directories(fc8_io PRIVATE src/)
target_compile_definitions(fc8_io PRIVATE KEYBOARD_$<UPPER_CASE:${FC8_KEYBOARD}>)

if(graphics STREQUAL x11)
    target_link_libraries(fc8_io "${X11_LIBRARIES}")
    target_include_directories(fc8_io PRIVATE "${X11_INCLUDE_DIR}")
endif()

if(graphics STREQUAL sdl2)
    target_include_directories(fc8_io PRIVATE "${SDL2_INCLUDE_DIRS}")
    target_link_libraries(fc8_io PRIVATE "${SDL2_LIBRARIES}")
endif()

if(graphics STREQUAL sfml)
    # Currently, broken
    target_link_libraries(fc8_io PRIVATE sfml-graphics sfml-window)
endif()

#
# Virtual machine
#
add_executable(fc8
    src/fc8.f90 
    src/fc8_vm.f90
    src/fc8_cmd.f90
    src/sleep_std.f90
    src/sleep.c
    src/randint8.c)

target_link_libraries(fc8 fc8_io)





