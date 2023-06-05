cmake_minimum_required(VERSION 3.20.2)

project(tridiagonal-solvers Fortran)

# Set missing CMake defaults for the NAG Fortran compiler
if(CMAKE_Fortran_COMPILER_ID MATCHES NAG)
  set(CMAKE_Fortran_FLAGS_DEBUG "-u -O0 -g -C=all -nan")
  set(CMAKE_Fortran_FLAGS_RELEASE "-u -O3")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${CMAKE_Fortran_FLAGS_RELEASE} -g")
endif()

# Disable assertions in release builds
add_compile_definitions($<$<CONFIG:Release,RelWithDebInfo>:NDEBUG>)

add_compile_options($<$<COMPILE_LANG_AND_ID:Fortran,Intel,IntelLLVM>:-standard-semantics>)

add_compile_definitions($<$<COMPILE_LANG_AND_ID:Fortran,NAG>:NAG_BUG20230603b>)
add_compile_definitions($<$<COMPILE_LANG_AND_ID:Fortran,Intel,IntelLLVM>:INTEL_BUG20230604>)

if(NOT CMAKE_BUILD_TYPE)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build." FORCE)
endif()

include(CTest)

add_subdirectory(src)
add_subdirectory(test)
#add_subdirectory(example)
