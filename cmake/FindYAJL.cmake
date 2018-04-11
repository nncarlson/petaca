# This module finds the YAJL library and include file directory.
# YAJL_FOUND is set to True if both are found, and the following
# variables are returned.
#
#  YAJL_INCLUDE_DIRS
#  YAJL_LIBRARIES
#  YAJL_VERSION
#
# This module defines the imported library target YAJL::YAJL.  It is
# generally enough to include YAJL::YAJL as a target link library; cmake
# will automatically handle adding the appropriate compile include flags
# and collection of link libraries.
#
# To provide the module with a hint about where to find your YAJL installation
# you have several options. You can include the root installation directory in
# the setting of the CMAKE_PREFIX_PATH variable, or you can set the environment
# variable YAJL_ROOT or the cmake variable YAJL_ROOT.

if(NOT YAJL_ROOT)
  set(YAJL_ROOT $ENV{YAJL_ROOT})
endif()
if(YAJL_ROOT)
  set(yajl_search_opts NO_DEFAULT_PATH)
else()
  set(yajl_search_opts)
endif()

find_path(YAJL_INCLUDE_DIR NAMES yajl/yajl_common.h
          HINTS ${YAJL_ROOT} ${yajl_search_opts} PATH_SUFFIXES include)
find_library(YAJL_LIBRARY NAMES yajl yajl_s
             HINTS ${YAJL_ROOT} ${yajl_search_opts} PATH_SUFFIXES lib)

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
  if(NOT TARGET YAJL::YAJL)
    add_library(YAJL::YAJL UNKNOWN IMPORTED)
    set_target_properties(YAJL::YAJL PROPERTIES
        IMPORTED_LOCATION "${YAJL_LIBRARY}"
        INTERFACE_INCLUDE_DIRECTORIES "${YAJL_INCLUDE_DIRS}"
        INTERFACE_LINK_LIBRARIES "${YAJL_LIBRARIES}")
  endif()
endif()
