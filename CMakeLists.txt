# ############################################################################ #
#                                                                              #
# Petaca (CMake)                                                               #
#                                                                              #
# ############################################################################ #

# ---------------------------------------------------------------------------- #
# Set policy and CMake version requirements
# ---------------------------------------------------------------------------- #
cmake_minimum_required(VERSION 3.3)

# ---------------------------------------------------------------------------- #
# Project definitions
# ---------------------------------------------------------------------------- #
project(Petaca Fortran C)

set(CMAKE_MODULE_PATH "${Petaca_SOURCE_DIR}/cmake/")

# The parameter list stuff requires the yajl library.
# If necessary set YAJL_ROOT to the yajl installation root.
find_package(YAJL REQUIRED)
if(YAJL_VERSION VERSION_LESS "2.0.1")
  message(FATAL_ERROR "libyajl 2.0.1 or later is required")
endif()

set(Petaca_INCLUDE_DIR ${Petaca_SOURCE_DIR}/include)

# ---------------------------------------------------------------------------- #
# Build Options 
# ---------------------------------------------------------------------------- #

option(ENABLE_TESTS "Build test programs" ON)
option(ENABLE_EXAMPLES "Build example programs" OFF)

#-------------------------------------------------------------------------------
# Compiler Definitions
#-------------------------------------------------------------------------------

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

if(CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -w=uda -DNDEBUG")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-g90")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -DNDEBUG -assume realloc_lhs")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-g -C -assume realloc_lhs")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -DNDEBUG -ffree-line-length-none")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -fbacktrace -ffree-line-length-none -fcheck=all -Wall -Wextra")
endif()

# Fortran preprocessor defines
set(Fortran_COMPILE_DEFINITIONS)
if(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "16.0.2")
    message(FATAL_ERROR "Intel Fortran 16.0.2 or later is required; "
        "your version is ${CMAKE_Fortran_COMPILER_VERSION}")
  elseif(CMAKE_Fortran_COMPILER_VERSION MATCHES "18.0.0.*")
    message(FATAL_ERROR "Intel Fortran version 18.0.0 is blacklisted; update to 18.0.1 or later")
  endif()
  list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_BUG20180115)
  list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_BUG20140921)
  list(APPEND Fortran_COMPILE_DEFINITIONS NO_2008_GENERIC_RESOLUTION)
  list(APPEND Fortran_COMPILE_DEFINITIONS NO_2008_LHS_POLY_REALLOC)
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER "18.0.1")
    list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_BUG20171115)
  endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES NAG)
  include(NAGFortranCompilerVersion) # set CMAKE_Fortran_COMPILER_VERSION
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "6.0.0")
    message(FATAL_ERROR "NAG Fortran 6.0 or later is required; "
        "your version is ${CMAKE_Fortran_COMPILER_VERSION}")
  endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  list(APPEND Fortran_COMPILE_DEFINITIONS GNU_PR93762)
  list(APPEND Fortran_COMPILE_DEFINITIONS NO_2008_GENERIC_RESOLUTION)
  if((CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL "10.1") AND
     (CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "10.2"))
    list(APPEND Fortran_COMPILE_DEFINITIONS GFORTRAN_10_1)
  endif()
endif()

# Append preprocessor defines to the compiler flags
foreach(def ${Fortran_COMPILE_DEFINITIONS})
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -D${def}")
endforeach()
message(STATUS "The Fortran compiler identification is "
    "${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}")
message(STATUS "CMAKE_Fortran_FLAGS=${CMAKE_Fortran_FLAGS}")

#-------------------------------------------------------------------------------
# Installation Definitions
#-------------------------------------------------------------------------------

add_subdirectory(src)

if(ENABLE_TESTS)
  enable_testing()
  add_subdirectory(test)
endif()

if(ENABLE_EXAMPLES)
  add_subdirectory(examples)
endif()

option(BUILD_HTML "Build HTML documentation (requires Sphinx)" OFF)
if(BUILD_HTML)
  find_package(Sphinx REQUIRED)
  add_subdirectory(doc/sphinx)
endif()
