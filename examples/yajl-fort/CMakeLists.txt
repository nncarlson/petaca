# Want example  modules in the build directory.
unset(CMAKE_Fortran_MODULE_DIRECTORY)

include_directories(${Petaca_MODULE_DIR})

# Add the include directory to the include search path.
include_directories(${Petaca_INCLUDE_DIR})

add_executable (yajl_fort_parse_example yajl_fort_parse_example.F90)
target_link_libraries(yajl_fort_parse_example petaca)
