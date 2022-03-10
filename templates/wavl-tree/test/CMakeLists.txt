# Want test modules in the build directory.
#unset(CMAKE_Fortran_MODULE_DIRECTORY)

add_executable(perf_test perf_test.F90)
target_link_libraries(perf_test common)
add_test(perf_test ./perf_test)
