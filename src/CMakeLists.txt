set(Petaca_SOURCE_FILES
      f90_assert.F90
      fortran_dynamic_loader.F90
      map_any_type.F90
      yajl_fort.F90
      yajl_ext.c
      parameter_entry_class.F90
      parameter_list_type.F90
      parameter_list_json.F90
      state_history_type.F90
      secure_hash/secure_hash_class.F90
      secure_hash/secure_hash_factory.F90
      secure_hash/md5_hash_type.F90
      secure_hash/sha1_hash_type.F90
      timer_tree_type.F90)

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  set_property (SOURCE secure_hash/md5_hash_type.F90
                       secure_hash/sha1_hash_type.F90
                PROPERTY COMPILE_FLAGS -fno-range-check)
endif()

include_directories(${Petaca_INCLUDE_DIR})
include_directories(${Petaca_MODULE_DIR})
include_directories(${YAJL_INCLUDE_DIR}) # for yajl_ext.c

add_library(petaca ${Petaca_SOURCE_FILES})

target_link_libraries(petaca dl) # for fortran_dynamic_loader
target_link_libraries(petaca yajl)  # for yajl-fort

install(TARGETS petaca DESTINATION lib)
        
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY} DESTINATION include)
