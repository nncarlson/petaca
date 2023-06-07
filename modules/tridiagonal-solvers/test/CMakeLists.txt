macro(add_serial_test test exe)
  add_executable(${exe} ${exe}.F90)
  target_link_libraries(${exe} serial-lib)
  add_test(${test} ./${exe})
endmacro()

add_serial_test(block_solver_procs block_solver_procs_test)
add_serial_test(td_matrix td_matrix_test)
add_serial_test(btd_matrix btd_matrix_test)

macro(add_parallel_test test exe nproc)
  if (CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    add_test(${test} cafrun -n ${nproc} ./${exe} ${ARGN})
  else()
    add_test(${test} ./${exe} ${ARGN})
    if (CMAKE_Fortran_COMPILER_ID MATCHES NAG)
      set_tests_properties(${test} PROPERTIES ENVIRONMENT NAGFORTRAN_NUM_IMAGES=${nproc})
    elseif(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
      set_tests_properties(${test} PROPERTIES ENVIRONMENT "FOR_COARRAY_NUM_IMAGES=${nproc}")
    endif()
  endif()
  set_tests_properties(${test} PROPERTIES PROCESSORS ${nproc})
endmacro()

add_executable(co_td_matrix_test co_td_matrix_test.F90)
target_link_libraries(co_td_matrix_test parallel-lib)

# This delegates to the serial td_matrix methods (1 image)
add_parallel_test(co_td_matrix_1 co_td_matrix_test 1 5)

# This follows the 1x1 and 2x2 non-periodic Schur code paths (2 images)
add_parallel_test(co_td_matrix_2 co_td_matrix_test 2 4)

# This follows the 2x2 non-periodic and 3x3 periodic Schur code paths (3 images)
add_parallel_test(co_td_matrix_3 co_td_matrix_test 3 6)

# This follows the code paths for the general case (num_images > 3)
add_parallel_test(co_td_matrix_4 co_td_matrix_test 4 20)

add_executable(co_btd_matrix_test co_btd_matrix_test.F90)
target_link_libraries(co_btd_matrix_test parallel-lib)

# This delegates to the serial btd_matrix methods (1 image)
add_parallel_test(co_btd_matrix_1 co_btd_matrix_test 1 2 5)

# This follows the 1x1 and 2x2 non-periodic Schur code paths (2 images)
add_parallel_test(co_btd_matrix_2 co_btd_matrix_test 2 2 4)

# This follows the 2x2 non-periodic and 3x3 periodic Schur code paths (3 images)
add_parallel_test(co_btd_matrix_3 co_btd_matrix_test 3 2 6)

# This follows the code paths for the general case (num_images > 3)
add_parallel_test(co_btd_matrix_4 co_btd_matrix_test 4 4 20)
