#[=======================================================================[.rst:
FindEggX
-------

Finds the EggX library.

Imported Targets
^^^^^^^^^^^^^^^^

This module provides the following imported targets, if found:

``EggX::EggX``
  The EggX/ProCALL library

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

find_path(EggX_INCLUDE_DIR eggxlib.h
    HINTS /usr/local/include
    REQUIRED)

find_library(EggX_LIBRARY eggx
    HINTS /usr/local/lib
    REQUIRED)

#find_library(X11_LIBRARY X11
#    HINTS /usr/local/lib
#    REQUIRED)

# /*
#  EGGX / ProCALL  version 0.95
#                        eggx.h
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
    #message(STATUS "version major: " ${EggX_VERSION_MAJOR})
    #message(STATUS "version minor: " ${EggX_VERSION_MINOR})
endif()


find_package_handle_standard_args(EggX
    REQUIRED_VARS 
        EggX_LIBRARY 
        EggX_INCLUDE_DIR
    VERSION_VAR EggX_VERSION)

find_package(X11 REQUIRED)

if(EggX_FOUND)
    set(EggX_LIBRARIES "${EggX_LIBRARY};${X11_LIBRARIES}")
    set(EggX_INCLUDE_DIRS ${EggX_INCLUDE_DIR})
endif()

# if (EggX_FOUND)
#     mark_as_advanced(EggX_INCLUDE_DIR)
#     mark_as_advanced(EggX_LIBRARY)
#     mark_as_advanced(EggX_VERSION)
# endif()

# if (EggX_FOUND AND NOT TARGET EggX::EggX)
#     add_library(EggX::EggX UNKNOWN IMPORTED)
#     set_property(TARGET EggX::EggX PROPERTY IMPORTED_LOCATION ${EggX_LIBRARY})
#     set_property(TARGET EggX::EggX PROPERTY VERSION ${EggX_VERSION})
#     target_include_directories(EggX::EggX INTERFACE ${EggX_INCLUDE_DIR})

#     # ToDo: Fix Handling of X11 library
#     target_link_libraries(EggX::EggX INTERFACE -lX11)
# endif()

message(STATUS "EggX_FOUND:        ${EggX_FOUND}" )
message(STATUS "EggX_VERSION:      ${EggX_VERSION}" )
message(STATUS "EggX_LIBRARIES:    ${EggX_LIBRARIES}" )
message(STATUS "EggX_INCLUDE_DIRS: ${EggX_INCLUDE_DIRS}" )
