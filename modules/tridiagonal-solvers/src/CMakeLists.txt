add_library(serial-lib td_matrix_type.F90 btd_matrix_type.F90 block_solver_procs.F90)
target_include_directories(serial-lib PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
)

add_library(parallel-lib co_td_matrix_type.F90 co_btd_matrix_type.F90)
target_link_libraries(parallel-lib PUBLIC serial-lib)
target_compile_options(parallel-lib PUBLIC $<$<COMPILE_LANG_AND_ID:Fortran,NAG,Intel,IntelLLVM>:-coarray>)
target_link_options(parallel-lib PUBLIC $<$<COMPILE_LANG_AND_ID:Fortran,NAG,Intel,IntelLLVM>:-coarray>)
target_include_directories(parallel-lib PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
)
