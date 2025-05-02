cmake_minimum_required(VERSION 3.20.2)

project(Petaca Fortran C)

set(CMAKE_MODULE_PATH "${Petaca_SOURCE_DIR}/cmake/")

# If necessary, set YAJL_ROOT to the yajl installation root
find_package(YAJL REQUIRED)
if(YAJL_VERSION VERSION_LESS "2.0.1")
  message(FATAL_ERROR "libyajl 2.0.1 or later is required")
endif()

# CMake build options
option(BUILD_EXAMPLES "Build example programs" OFF)
option(BUILD_HTML "Build HTML documentation (requires Sphinx)" OFF)
option(BUILD_SHARED_LIBS "Build shared object library" ON)
option(ENABLE_STD_MOD_PROC_NAME "Build with -assume std_mod_proc_name when using Intel" OFF)

# Some Fortran compilers may not support 128-bit reals
include(CheckFortranSourceCompiles)
check_fortran_source_compiles("use::iso_fortran_env;real(real128)::x;end" real128 SRC_EXT ".f90")
if(real128)
  add_compile_definitions($<$<COMPILE_LANGUAGE:Fortran>:HAVE_REAL128>)
endif()

include(CTest) # defines option BUILD_TESTING (on)

if(NOT CMAKE_BUILD_TYPE)
  message(STATUS "Setting build type to 'Release' as none was specified")
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build" FORCE)
endif()

# Set missing CMake defaults for the NAG Fortran compiler
if(CMAKE_Fortran_COMPILER_ID STREQUAL NAG)
  set(CMAKE_Fortran_FLAGS_DEBUG "-u -O0 -gline -C -nan")
  set(CMAKE_Fortran_FLAGS_RELEASE "-u -O3")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${CMAKE_Fortran_FLAGS_RELEASE} -g")
endif()

set(Petaca_INCLUDE_DIR ${Petaca_SOURCE_DIR}/include)

# Disable assertions in release builds
add_compile_definitions($<$<CONFIG:Release,RelWithDebInfo>:NDEBUG>)

# Compiler bug workarounds
add_compile_definitions(
  $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:INTEL_BUG20231123>
  $<$<COMPILE_LANG_AND_ID:Fortran,IntelLLVM>:INTEL_BUG20231122>
  $<$<COMPILE_LANG_AND_ID:Fortran,IntelLLVM>:INTEL_BUG20231123>
  $<$<COMPILE_LANG_AND_ID:Fortran,Intel,IntelLLVM>:INTEL_BUG20231205>
  $<$<COMPILE_LANG_AND_ID:Fortran,Intel,IntelLLVM>:INTEL_BUG20231210>
  $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:GNU_PR93762>
  $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:GNU_PR112964>
  $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:GNU_PR114827>
  $<$<COMPILE_LANG_AND_ID:Fortran,NAG>:NAG_BUG20231204>
  $<$<COMPILE_LANG_AND_ID:Fortran,NAG>:NAG_BUG20231206>
)

# Required compiler options
add_compile_options($<$<COMPILE_LANG_AND_ID:Fortran,NAG>:-f2018>)
add_compile_options($<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-ffree-line-length-none>)
add_compile_options("$<$<COMPILE_LANG_AND_ID:Fortran,Intel,IntelLLVM>:-standard-semantics>"
  "$<$<AND:$<NOT:$<BOOL:${ENABLE_STD_MOD_PROC_NAME}>>,$<COMPILE_LANG_AND_ID:Fortran,Intel,IntelLLVM>>:SHELL:-assume nostd_mod_proc_name>")

add_subdirectory(src)

if(BUILD_TESTING)
  add_subdirectory(test)
endif()

if(BUILD_EXAMPLES)
  add_subdirectory(examples)
endif()

if(BUILD_HTML)
  find_package(Sphinx REQUIRED)
  add_subdirectory(doc/sphinx)
endif()

add_subdirectory(templates)
