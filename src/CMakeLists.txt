set(Petaca_SOURCE_FILES
      f90_assert.F90
      fortran_dynamic_loader.F90
      map_any_type.F90
      yajl_fort.F90
      yajl_ext.c
      json.F90
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

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod_files/)

include_directories(${YAJL_INCLUDE_DIR}) # for yajl_ext.c

add_library(petaca ${Petaca_SOURCE_FILES})

target_link_libraries(petaca PUBLIC yajl dl)

target_include_directories(petaca PUBLIC
    $<BUILD_INTERFACE:${Petaca_INCLUDE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
    $<INSTALL_INTERFACE:include>
)

install(TARGETS petaca
        EXPORT  truchas
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY} DESTINATION include)
