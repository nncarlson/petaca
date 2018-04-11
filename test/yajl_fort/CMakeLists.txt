add_executable(json-strip strip.F90)
target_link_libraries(json-strip petaca)

add_test(NAME strip-file1 COMMAND
    ${CMAKE_CURRENT_SOURCE_DIR}/strip-cmp.sh json-strip
    ${CMAKE_CURRENT_SOURCE_DIR}/file1.json)
