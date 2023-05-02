cmake_minimum_required(VERSION 3.21)

project(FC8
        VERSION 0.1.0
        DESCRIPTION "Fortran CHIP-8 interpreter"
        HOMEPAGE_URL "https://github.com/ivan-pi/FC8"
        LANGUAGES C Fortran)

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

find_package(EGGX 0.95 REQUIRED)

add_library(eggx-procall-2003 OBJECT procall.f90)
target_link_libraries(eggx-procall-2003 PUBLIC ${EGGX_LIBRARIES})

add_executable(fc8 chip8.F90 rand_byte.c)
target_link_libraries(fc8 eggx-procall-2003)