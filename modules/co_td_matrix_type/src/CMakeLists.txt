add_library(common OBJECT co_td_matrix_type.F90)
target_include_directories(common PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
)
target_compile_options(common PUBLIC $<$<COMPILE_LANG_AND_ID:Fortran,NAG,Intel,IntelLLVM>:-coarray>)
target_link_options(common PUBLIC $<$<COMPILE_LANG_AND_ID:Fortran,NAG,Intel,IntelLLVM>:-coarray>)
