# ############################################################################ #
#                                                                              #
# Petaca (CMake)                                                               #
#                                                                              #
# ############################################################################ #

# ---------------------------------------------------------------------------- #
# Set policy and CMake version requirements
# ---------------------------------------------------------------------------- #
cmake_minimum_required( VERSION 2.8.8 )

# allow more human readable "if then else" constructs
set( CMAKE_ALLOW_LOOSE_LOOP_CONSTRUCTS TRUE )

# Modules/Compiler/NAG-Fortran.cmake is missing this.
if(CMAKE_Fortran_COMPILER_ID MATCHES NAG)
  set(CMAKE_Fortran_COMPILE_OPTIONS_PIC "-PIC")
endif()

# ---------------------------------------------------------------------------- #
# Project definitions
# ---------------------------------------------------------------------------- #
project(Petaca Fortran)

# Create all the Fortran module files in a directory at the root build directory
set(Petaca_MODULE_DIR ${Petaca_BINARY_DIR}/mod_files/)
set(CMAKE_Fortran_MODULE_DIRECTORY ${Petaca_MODULE_DIR})

# ---------------------------------------------------------------------------- #
# Build Options 
# ---------------------------------------------------------------------------- #

# Build the tests
if (ENABLE_TESTS)
  enable_testing()
endif()

#-------------------------------------------------------------------------------
# Installation Definitions
#-------------------------------------------------------------------------------

add_subdirectory(src)
