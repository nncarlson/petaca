set(Petaca_SOURCE_FILES
      f90_assert.F90
      fortran_dynamic_loader.F90
      map_any_type.F90
      yajl_fort.F90
      parameter_entry_type.F90
      parameter_list_type.F90
      parameter_list_json.F90
      state_history_type.F90)
                            
                            
include_directories(${Petaca_INCLUDE_DIR})
include_directories(${Petaca_MODULE_DIR})

add_library(petaca ${Petaca_SOURCE_FILES})

target_link_libraries(petaca dl) # for fortran_dynamic_loader
target_link_libraries(petaca yajl)  # for yajl-fort

install(TARGETS petaca DESTINATION lib)
        
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY} DESTINATION include)
