add_executable (test_timer_tree test_timer_tree.F90)
target_link_libraries(test_timer_tree petaca)
add_test(timer_tree test_timer_tree)
