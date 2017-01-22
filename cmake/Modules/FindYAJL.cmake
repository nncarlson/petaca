# This module finds the YAJL library and include file directory.
# YAJL_FOUND is set to True if both are found, and the following
# variables are returned.
#
#  YAJL_INCLUDE_DIRS
#  YAJL_LIBRARIES
#  YAJL_VERSION
#
# This module also defines the imported library target "yajl".  It is
# generally enough to include "yajl" as a target link library; cmake
# will automatically handle adding the appropriate compile include flags
# and collection of link libraries.
#
# Set the variable CMAKE_PREFIX_PATH to provide a hint to the module for
# where to find the library and header file.  This is searched before the
# standard system locations.

find_path(YAJL_INCLUDE_DIR yajl/yajl_common.h)
find_library(YAJL_LIBRARY NAMES yajl yajl_s) 

if(NOT YAJL_VERSION)
  if(YAJL_INCLUDE_DIR AND YAJL_LIBRARY)
    set(yajl_version_h ${YAJL_INCLUDE_DIR}/yajl/yajl_version.h)
    include(SearchHeaderFile)
    search_header_file(${yajl_version_h} "YAJL_MAJOR" _major)
    search_header_file(${yajl_version_h} "YAJL_MINOR" _minor)
    search_header_file(${yajl_version_h} "YAJL_MICRO" _micro)
    set(YAJL_VERSION ${_major}.${_minor}.${_micro})
    unset(_major)
    unset(_minor)
    unset(_micro)
    unset(yajl_version_h)
  else()
    set(YAJL_VERSION YAJL_VERSION-NOTFOUND)
  endif()
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(YAJL
    REQUIRED_VARS YAJL_LIBRARY YAJL_INCLUDE_DIR 
    VERSION_VAR YAJL_VERSION)

if(YAJL_FOUND)
  set(YAJL_INCLUDE_DIRS ${YAJL_INCLUDE_DIR})
  set(YAJL_LIBRARIES ${YAJL_LIBRARY})
  mark_as_advanced(YAJL_INCLUDE_DIR YAJL_LIBRARY)
  if(NOT TARGET yajl)
    add_library(yajl UNKNOWN IMPORTED)
    set_target_properties(yajl PROPERTIES
        IMPORTED_LOCATION "${YAJL_LIBRARY}"
        INTERFACE_INCLUDE_DIRECTORIES "${YAJL_INCLUDE_DIRS}"
        INTERFACE_LINK_LIBRARIES "${YAJL_LIBRARIES}")
  endif()
endif()
