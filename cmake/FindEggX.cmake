#[=======================================================================[.rst:
FindEGGX
-------

Finds the EGGX library.

Options
^^^^^^^

``EGGX_DIR``
  Hint directory for a custom EGGX/ProCALL installation.

Result Variables
^^^^^^^^^^^^^^^^

This will define the following variables:

``EGGX_FOUND``
  True if the system has the EGGX library.
``EGGX_VERSION``
  The version of the EGGX library which was found.
``EGGX_INCLUDE_DIRS``
  Include directories needed to use EGGX.
``EGGX_LIBRARIES``
  Libraries needed to link to Foo.

Cache Variables
^^^^^^^^^^^^^^^

The following cache variables may also be set:

``EGGX_INCLUDE_DIR``
  The directory containing ``eggx.h`` and ``eggxlib.h``.
``EGGX_LIBRARY``
  The path to the EGGX library.

#]=======================================================================]

include(FindPackageHandleStandardArgs)

find_package(X11 REQUIRED)

if(EGGX_DIR)
    message(STATUS "Using suggested EGGX_DIR: " ${EGGX_DIR})
endif()


find_path(EGGX_INCLUDE_DIR eggxlib.h
    HINTS /usr/local/include ${EGGX_DIR}/include 
    REQUIRED)

find_library(EGGX_LIBRARY eggx
    HINTS /usr/local/lib ${EGGX_DIR}/lib
    REQUIRED)

# For now, we extract the version number from the EGGX header file
# which contains the following comment:
#
#/*
#  EGGX / ProCALL  version 0.95
#                     eggxlib.h
# */
if (EGGX_INCLUDE_DIR)
    file(STRINGS "${EGGX_INCLUDE_DIR}/eggxlib.h" header-file
        REGEX "  EGGX / ProCALL  version [0-9]+\\.[0-9]+")
    if (NOT header-file)
        message(AUTHOR_WARNING "EGGX_INCLUDE_DIR found, but eggxlib.h is missing")
    endif()
    string(REGEX REPLACE 
        "  EGGX / ProCALL  version ([0-9]+)\\.([0-9]+)" 
        "\\1" 
        EGGX_VERSION_MAJOR 
        ${header-file})
    string(REGEX REPLACE 
        "  EGGX / ProCALL  version ([0-9]+)\\.([0-9]+)" 
        "\\2" 
        EGGX_VERSION_MINOR 
        ${header-file})
    set(EGGX_VERSION "${EGGX_VERSION_MAJOR}.${EGGX_VERSION_MINOR}" CACHE STRING "EGGX Version")
endif()

find_package_handle_standard_args(EGGX
    REQUIRED_VARS 
        EGGX_LIBRARY 
        EGGX_INCLUDE_DIR
    VERSION_VAR EGGX_VERSION)

if(EGGX_FOUND)
    set(EGGX_LIBRARIES "${EGGX_LIBRARY};${X11_LIBRARIES}")
    set(EGGX_INCLUDE_DIRS ${EGGX_INCLUDE_DIR})
endif()