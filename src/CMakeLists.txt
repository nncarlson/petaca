set(SRC
    f90_assert.F90
    fortran_dynamic_loader.F90
    map_any_type.F90
    yajl_fort.F90
    yajl_ext.c
    json.F90
    parameter_list_type.F90
    parameter_list_json.F90
    state_history_type.F90
    secure_hash/secure_hash_class.F90
    secure_hash/secure_hash_factory.F90
    secure_hash/md5_hash_type.F90
    secure_hash/sha1_hash_type.F90
    timer_tree_type.F90
)

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  set_property (SOURCE secure_hash/md5_hash_type.F90
                       secure_hash/sha1_hash_type.F90
                PROPERTY COMPILE_FLAGS -fno-range-check)
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
  set_property(SOURCE parameter_list_json.F90
               PROPERTY COMPILE_FLAGS "-assume old_e0g0_format")
endif()

add_library(petaca ${SRC})

set(LIB_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod_files/)
set_target_properties(petaca PROPERTIES Fortran_MODULE_DIRECTORY ${LIB_MOD_DIR})

target_link_libraries(petaca PUBLIC YAJL::YAJL dl)

target_include_directories(petaca PUBLIC
    $<BUILD_INTERFACE:${Petaca_INCLUDE_DIR}>
    $<BUILD_INTERFACE:${LIB_MOD_DIR}>
    $<INSTALL_INTERFACE:lib>
)

install(TARGETS petaca
        EXPORT  petaca
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
)
install(DIRECTORY ${LIB_MOD_DIR} DESTINATION lib)
