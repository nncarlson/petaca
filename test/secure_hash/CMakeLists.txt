add_executable (test_secure_hash test_secure_hash.F90)
target_link_libraries(test_secure_hash petaca)
add_test(secure_hash test_secure_hash)

#add_executable (time_hash time_hash.F90)
#target_link_libraries(time_hash petaca)

### Preprocessor defines; mainly compiler bug workarounds.
foreach (def ${Fortran_COMPILE_DEFINITIONS})
  set_property (TARGET test_secure_hash APPEND PROPERTY COMPILE_DEFINITIONS ${def})
endforeach ()
