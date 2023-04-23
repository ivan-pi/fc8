cmake_minimum_required(VERSION 3.21)

project(FC8
        VERSION 0.1.0
        DESCRIPTION "Fortran CHIP-8 interpreter"
        HOMEPAGE_URL "https://github.com/ivan-pi/FC8"
        LANGUAGES C Fortran)

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

find_package(EGGX 0.95 REQUIRED)

add_library(eggx-procall-2003 OBJECT src/procall.f90)
target_link_libraries(eggx-procall-2003 PUBLIC ${EGGX_LIBRARIES})

add_executable(FC8 
    src/FC8.f90 
    src/FC8_vm.F90
    src/randint8.c
    )
target_link_libraries(FC8 eggx-procall-2003)

find_package(X11 REQUIRED)

if(X11_FOUND)
    add_executable(FC8-Xlib
        src/FC8-Xlib.f90 
        src/FC8_vm-Xlib.F90
        src/fcw-Xlib.f90
        sandbox/fcw_Xlib.c
        src/randint8.c
        src/sleep.c
        src/sleep_std.f90
        )
    target_link_libraries(FC8-Xlib "${X11_LIBRARIES}")
    target_include_directories(FC8-Xlib PRIVATE "${X11_INCLUDE_DIR}")
endif()