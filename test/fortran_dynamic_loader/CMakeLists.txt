add_executable (test_fortran_dynamic_loader test_fortran_dynamic_loader.F90)
target_link_libraries(test_fortran_dynamic_loader petaca)
add_test(fortran_dynamic_loader test_fortran_dynamic_loader)

# Shared library loaded by the test.
add_library(mylib SHARED mylib.F90)
