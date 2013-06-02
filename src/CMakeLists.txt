set(FDL_SOURCE_FILES fdl/fortran_dynamic_loader.F90)
set(MAT_SOURCE_FILES map_any_type/map_any_type.F90)

# sum up all the preceding source files
set(Petaca_SOURCE_FILES ${FDL_SOURCE_FILES} ${MAT_SOURCE_FILES})

add_library(petaca ${Petaca_SOURCE_FILES})

# for fdl
target_link_libraries(petaca dl)

install(TARGETS petaca DESTINATION lib)
        
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY} DESTINATION include)

if(ENABLE_TESTS)
  add_subdirectory(fdl/test)
  add_subdirectory(map_any_type/test)
endif()

if(ENABLE_EXAMPLES)
  add_subdirectory(map_any_type/examples)
endif()
