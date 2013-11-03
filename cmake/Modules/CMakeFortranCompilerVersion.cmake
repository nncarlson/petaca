#
#  Determine the Fortran compiler version
#
#  CMAKE_Fortran_COMPILER_VERSION -- the version string (x.y.z)
#

if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
  execute_process (COMMAND ${CMAKE_Fortran_COMPILER} --version OUTPUT_VARIABLE CMAKE_Fortran_COMPILER_VERSION)
  string (REGEX MATCH "[0-9]+.[0-9]+.[0-9]+" CMAKE_Fortran_COMPILER_VERSION ${CMAKE_Fortran_COMPILER_VERSION})
endif ()
