macro(add_serial_test test exe)
  add_executable(${exe} ${exe}.F90)
  target_link_libraries(${exe} common)
  add_test(${test} ./${exe})
endmacro()

add_serial_test(td_matrix td_matrix_test)
