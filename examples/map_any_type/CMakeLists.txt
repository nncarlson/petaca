# Want example  modules in the build directory.
unset(CMAKE_Fortran_MODULE_DIRECTORY)

# Add the library module directory to the module search path.
include_directories(${Petaca_MODULE_DIR})

# Add the include directory to the include search path.
include_directories(${Petaca_INCLUDE_DIR})

add_executable (example example.F90)
target_link_libraries(example petaca)
