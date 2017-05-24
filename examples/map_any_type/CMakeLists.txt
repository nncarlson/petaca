add_executable (example example.F90)
target_link_libraries(example petaca)

### Preprocessor defines; mainly compiler bug workarounds.
foreach (def ${Fortran_COMPILE_DEFINITIONS})
  set_property (TARGET example APPEND PROPERTY COMPILE_DEFINITIONS ${def})
endforeach ()
