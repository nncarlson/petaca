# Want test modules in the build directory.
unset(CMAKE_Fortran_MODULE_DIRECTORY)

# Add the library module directory to the module search path.
include_directories(${Petaca_MODULE_DIR})
include_directories(${Petaca_INCLUDE_DIR})

add_executable (test_timer_tree test_timer_tree.F90)
target_link_libraries(test_timer_tree petaca)
add_test(timer_tree test_timer_tree)
