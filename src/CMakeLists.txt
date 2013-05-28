set(FDL_SOURCE_FILES fdl/fortran_dynamic_loader.F90)

# sum up all the preceding source files
set(Petaca_SOURCE_FILES ${FDL_SOURCE_FILES})

add_library(petaca ${Petaca_SOURCE_FILES})

# for fdl
target_link_libraries(petaca dl)

install(TARGETS petaca DESTINATION lib)
        
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY} DESTINATION include)

if(ENABLE_TESTS)
  add_subdirectory(fdl/test)
endif()
