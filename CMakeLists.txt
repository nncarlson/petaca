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

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${Petaca_SOURCE_DIR}/cmake/Modules/")

# The parameter list stuff requires the yajl library.
# If necessary set YAJL_INCLUDE_DIR and YAJL_LIBRARY_DIR.
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
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "13.1.0")
    message(FATAL_ERROR "ERROR: Intel Fortran 13.1.0 or later is required; "
        "your version is ${CMAKE_Fortran_COMPILER_VERSION}")
  elseif(CMAKE_Fortran_COMPILER_VERSION MATCHES "18.0.0.*")
    message(FATAL_ERROR "Intel Fortran version 18.0.0 is blacklisted; update to 18.0.1 or later")
  endif()
  list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_BUG20140921)
  list(APPEND Fortran_COMPILE_DEFINITIONS NO_2008_GENERIC_RESOLUTION)
  list(APPEND Fortran_COMPILE_DEFINITIONS NO_2008_LHS_POLY_REALLOC)
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER "18.0.1")
    list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_BUG20171115)
  endif()
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "16.0.0")
    list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_DPD200357656)
  endif()
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "15.0.3")
    list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_DPD200357694)
    list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_DPD200357693)
    list(APPEND Fortran_COMPILE_DEFINITIONS INTEL_CHECK_BUG)
    list(APPEND Fortran_COMPILE_DEFINITIONS NO_2008_PTR_FUN_RESULT_IS_VAR)
  endif()
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
elseif(CMAKE_Fortran_COMPILER_ID MATCHES NAG)
  include(NAGFortranCompilerVersion) # set CMAKE_Fortran_COMPILER_VERSION
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "5.3.1")
    message(FATAL_ERROR "NAG Fortran versions prior to 5.3.1 are not supported; "
        "your version is ${CMAKE_Fortran_COMPILER_VERSION}")
  endif()
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "6.0.0")
    list(APPEND Fortran_COMPILE_DEFINITIONS NO_2008_SOURCED_ALLOC_ARRAY)
    list(APPEND Fortran_COMPILE_DEFINITIONS NO_2008_LHS_POLY_REALLOC)
  endif()
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "5.3.2.990")
    list(APPEND Fortran_COMPILE_DEFINITIONS NAG_88538)
    list(APPEND Fortran_COMPILE_DEFINITIONS NAG_88549)
  endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "6.0.0")
    list(APPEND Fortran_COMPILE_DEFINITIONS NO_2008_SOURCED_ALLOC_ARRAY)
  endif()
  list(APPEND Fortran_COMPILE_DEFINITIONS NO_2008_GENERIC_RESOLUTION)
  list(APPEND Fortran_COMPILE_DEFINITIONS NO_2008_LHS_POLY_REALLOC)
  list(APPEND Fortran_COMPILE_DEFINITIONS GNU_67564)
  list(APPEND Fortran_COMPILE_DEFINITIONS GNU_61767)
  # Eliminate spurious runtime check errors caused by GCC gug PR67505
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -fcheck=no-recursion")
  # GFortran warning
  if(NOT WIN32)
    string(ASCII 27 esc)
    set(BoldRed "${esc}[1;31m")
    set(ResetColor "${esc}[m")
  endif()
  message(WARNING "${BoldRed}"
      "GFortran ${CMAKE_Fortran_COMPILER_VERSION} has incomplete/flawed support of "
      "finalization (PR37336), unlimited polymorphic variables (PR67564), and "
      "deferred-length character variables (PR68241), which may result in the "
      "code not executing properly; see https://gcc.gnu.org/bugzilla/query.cgi."
      "${ResetColor}")
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
