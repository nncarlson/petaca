#
# Determine the NAG Fortran compiler version.  It returns the variable
#
#  CMAKE_Fortran_COMPILER_VERSION -- the version string (x.y.z.b)
#
# Here b is the build number.  Cmake automatically sets this variable
# for other compilers, but the method it uses (extracting it from an
# object file compiled with the compiler) doesn't work for the NAG
# compiler.  Here the version is obtained from the output of running
# the compiler with the "-version" flag. 

if(CMAKE_Fortran_COMPILER_ID MATCHES NAG)
  if(NOT CMAKE_Fortran_COMPILER_VERSION)
    execute_process(COMMAND ${CMAKE_Fortran_COMPILER} -V ERROR_VARIABLE _info)
    if(${_info} MATCHES "([0-9]+)\\.([0-9]+)\\.?([0-9]+)?")
      set(_major "${CMAKE_MATCH_1}")
      set(_minor "${CMAKE_MATCH_2}")
      if(${CMAKE_MATCH_3})
        set(_patch "${CMAKE_MATCH_3}")
      else()
        set(_patch 0)
      endif()
      if(${_info} MATCHES "\\(([0-9]+)\\)")
        set(_build ${CMAKE_MATCH_1})
      elseif(${_info} MATCHES "Build ([0-9]+)\n")
        set(_build ${CMAKE_MATCH_1})
      else()
        set(_build)
      endif()
      set(CMAKE_Fortran_COMPILER_VERSION "${_major}.${_minor}.${_patch}")
      if(_build)
        set(CMAKE_Fortran_COMPILER_VERSION "${CMAKE_Fortran_COMPILER_VERSION}.${_build}")
      endif()
    endif()
  endif()
endif()
