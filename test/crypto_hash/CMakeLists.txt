project(CryptoHashTest)

include(CTest)

# Want test modules in the build directory.
unset(CMAKE_Fortran_MODULE_DIRECTORY)

# Add the library module directory to the module search path.
include_directories(${Petaca_MODULE_DIR})
include_directories(${Petaca_INCLUDE_DIR})

add_executable (test_crypto_hash test_crypto_hash.F90)
target_link_libraries(test_crypto_hash petaca)
add_test(crypto_hash test_crypto_hash)
