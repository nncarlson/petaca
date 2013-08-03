project(ParameterListTypeTest)

include(CTest)

# Want test modules in the build directory.
unset(CMAKE_Fortran_MODULE_DIRECTORY)

# Add the library module directory to the module search path.
include_directories(${Petaca_MODULE_DIR})
include_directories(${Petaca_INCLUDE_DIR})

add_executable (test_any_scalar_type test_any_scalar_type.F90)
target_link_libraries(test_any_scalar_type petaca)

add_executable (test_any_vector_type test_any_vector_type.F90)
target_link_libraries(test_any_vector_type petaca)

#add_executable (test_parameter_list_type test_parameter_list_type.F90)
#target_link_libraries(test_parameter_list_type petaca)
