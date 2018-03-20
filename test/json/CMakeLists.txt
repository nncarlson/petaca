# Tests for json.F90; also exercise yajl_fort.F90
foreach(n RANGE 1 9)
  add_executable(json-ex${n} json-ex${n}.f90)
  target_link_libraries(json-ex${n} petaca)
  add_test(json-ex${n} json-ex${n})
endforeach()
