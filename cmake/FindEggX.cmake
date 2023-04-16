#[=======================================================================[.rst:
FindEggX
-------

Finds the EggX library.

Result Variables
^^^^^^^^^^^^^^^^

This will define the following variables:

``EggX_FOUND``
  True if the system has the EggX library.
``EggX_VERSION``
  The version of the EggX library which was found.
``EggX_INCLUDE_DIRS``
  Include directories needed to use EggX.
``EggX_LIBRARIES``
  Libraries needed to link to Foo.

Cache Variables
^^^^^^^^^^^^^^^

The following cache variables may also be set:

``EggX_INCLUDE_DIR``
  The directory containing ``eggx.h`` and ``eggxlib.h``.
``EggX_LIBRARY``
  The path to the EggX library.

#]=======================================================================]

include(FindPackageHandleStandardArgs)

find_package(X11 REQUIRED)

find_path(EggX_INCLUDE_DIR eggxlib.h
    HINTS /usr/local/include
    REQUIRED)

find_library(EggX_LIBRARY eggx
    HINTS /usr/local/lib
    REQUIRED)

# For now, we extract the version number from the EggX header file
# which contains the following comment:
#
#/*
#  EGGX / ProCALL  version 0.95
#                     eggxlib.h
# */
if (EggX_INCLUDE_DIR)
    file(STRINGS "${EggX_INCLUDE_DIR}/eggxlib.h" header-file
        REGEX "  EGGX / ProCALL  version [0-9]+\\.[0-9]+")
    if (NOT header-file)
        message(AUTHOR_WARNING "EggX_INCLUDE_DIR found, but eggxlib.h is missing")
    endif()
    string(REGEX REPLACE 
        "  EGGX / ProCALL  version ([0-9]+)\\.([0-9]+)" 
        "\\1" 
        EggX_VERSION_MAJOR 
        ${header-file})
    string(REGEX REPLACE 
        "  EGGX / ProCALL  version ([0-9]+)\\.([0-9]+)" 
        "\\2" 
        EggX_VERSION_MINOR 
        ${header-file})
    set(EggX_VERSION "${EggX_VERSION_MAJOR}.${EggX_VERSION_MINOR}" CACHE STRING "EggX Version")
endif()

find_package_handle_standard_args(EggX
    REQUIRED_VARS 
        EggX_LIBRARY 
        EggX_INCLUDE_DIR
    VERSION_VAR EggX_VERSION)

if(EggX_FOUND)
    set(EggX_LIBRARIES "${EggX_LIBRARY};${X11_LIBRARIES}")
    set(EggX_INCLUDE_DIRS ${EggX_INCLUDE_DIR})
endif()