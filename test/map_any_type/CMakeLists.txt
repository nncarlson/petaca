project(MapAnyTypeTest)

include(CTest)

# Want test modules in the build directory.
unset(CMAKE_Fortran_MODULE_DIRECTORY)

# Add the library module directory to the module search path.
include_directories(${Petaca_MODULE_DIR})
include_directories(${Petaca_INCLUDE_DIR})

add_executable (test_map_any_type test_map_any_type.F90)
target_link_libraries(test_map_any_type petaca)
add_test(map_any test_map_any_type)
