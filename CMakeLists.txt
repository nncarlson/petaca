# ############################################################################ #
#                                                                              #
# Petaca (CMake)                                                               #
#                                                                              #
# ############################################################################ #

# ---------------------------------------------------------------------------- #
# Set policy and CMake version requirements
# ---------------------------------------------------------------------------- #
cmake_minimum_required( VERSION 2.8.12 )

# ---------------------------------------------------------------------------- #
# Project definitions
# ---------------------------------------------------------------------------- #
project (Petaca Fortran C)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${Petaca_SOURCE_DIR}/cmake/Modules/")

# The parameter list stuff requires the yajl library.
# If necessary set YAJL_INCLUDE_DIR and YAJL_LIBRARY_DIR.
find_package (YAJL REQUIRED)
if ("${YAJL_VERSION_STRING}" VERSION_LESS "2.0.1")
  message(FATAL_ERROR "ERROR! version 2.0.1 or later of the yajl library is required")
endif ()

# Create all the Fortran module files in a directory at the root build directory
set(Petaca_MODULE_DIR ${Petaca_BINARY_DIR}/mod_files/)
set(CMAKE_Fortran_MODULE_DIRECTORY ${Petaca_MODULE_DIR})

set(Petaca_INCLUDE_DIR ${Petaca_SOURCE_DIR}/include)

# ---------------------------------------------------------------------------- #
# Build Options 
# ---------------------------------------------------------------------------- #

option(ENABLE_TESTS "Build test programs" ON)
option(ENABLE_EXAMPLES "Build example programs" ON)

# Build the tests
if (ENABLE_TESTS)
  enable_testing()
endif()

#-------------------------------------------------------------------------------
# Compiler Definitions
#-------------------------------------------------------------------------------

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

message(STATUS "CMAKE_Fortran_COMPILER_ID=${CMAKE_Fortran_COMPILER_ID}")
if (CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -w=uda -DNDEBUG")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-g90")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 -DNDEBUG -assume realloc_lhs")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-g -assume realloc_lhs")
  message(STATUS "${CMAKE_Fortran_COMPILER_VERSION}")
endif()

# Fortran preprocessor defines
set(Fortran_COMPILE_DEFINITIONS)
include(CMakeFortranCompilerVersion)
if(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
  list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_BUG20140921)
  list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_DPD200357694)
  list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_DPD200357693)
  list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_DPD200357656)
  list(APPEND Fortran_COMPILE_DEFINITIONS NO_2008_GENERIC_RESOLUTION)
  list(APPEND Fortran_COMPILE_DEFINITIONS NO_2008_PTR_FUN_RESULT_IS_VAR)
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "15.0.0")
    list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_DPD200255963)
    list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_DPD200249493)
    list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_DPD200237118)
    list(APPEND Fortran_COMPILE_DEFINITIONS NO_2008_EXECUTE_COMMAND_LINE)
  endif()
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "14.0.0")
    list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_DPD200237219)
    list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_DPD200237121)
    list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_DPD200242779)
  endif()
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "13.1.1")
    message(FATAL_ERROR "ERROR: Intel Fortran 13.1.1 or later is required; "
            "your version is ${CMAKE_Fortran_COMPILER_VERSION}")
  endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES NAG)
  # CMAKE_Fortran_COMPILER_VERSION not set for NAG
  #message(STATUS "CMAKE_Fortran_COMPILER_VERSION=${CMAKE_Fortran_COMPILER_VERSION}")
  list(APPEND Fortran_COMPILE_DEFINITIONS NO_2008_SOURCED_ALLOC_ARRAY)
  list(APPEND Fortran_COMPILE_DEFINITIONS NAG_88538)
  list(APPEND Fortran_COMPILE_DEFINITIONS NAG_88549)
endif()

#-------------------------------------------------------------------------------
# Installation Definitions
#-------------------------------------------------------------------------------

add_subdirectory(src)

if(ENABLE_TESTS)
  add_subdirectory(test)
endif()

if(ENABLE_EXAMPLES)
  add_subdirectory(examples)
endif()
