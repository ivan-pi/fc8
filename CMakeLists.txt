cmake_minimum_required(VERSION 3.15 FATAL_ERROR)

project(FC8
        VERSION 0.1.0
        DESCRIPTION "Fortran CHIP-8 interpreter"
        HOMEPAGE_URL "https://github.com/ivan-pi/FC8"
        LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

find_package(EggX 0.95 REQUIRED)

add_library(EggX_ProCALL_2003 OBJECT 
    ../eggx-procall-2003/src/procall.f90
    ../eggx-procall-2003/src/eggx.f90)
target_link_libraries(EggX_ProCALL_2003 PUBLIC ${EggX_LIBRARIES})

add_executable(fc8 chip8.F90 randint8.c)
target_link_libraries(fc8 EggX_ProCALL_2003)