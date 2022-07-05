all:
	rm -f ./tower; dune build && ln -s ./_build/default/src/main.exe ./tower

test:
	dune build src/test && ./_build/default/src/test/test.exe

clean:
	dune clean && rm -f ./tower

check:
	./tower -i tests/list.twr tests/length.twr tests/stack.twr tests/stack_len_tests.twr
	./tower -i --inline tests/list.twr tests/length.twr tests/stack.twr tests/stack_len_tests.twr
	./tower -c --interp_lir tests/list.twr tests/length.twr tests/stack.twr tests/stack_len_tests.twr
	./tower -c --interp_circuit tests/list.twr tests/length.twr tests/stack.twr tests/stack_len_tests.twr
	./tower -c --interp_prim tests/list.twr tests/length.twr tests/stack.twr tests/stack_len_tests.twr
	./tower -i tests/list.twr tests/length_inefficient.twr tests/stack.twr tests/stack_len_tests.twr
	./tower -i --inline tests/list.twr tests/length_inefficient.twr tests/stack.twr tests/stack_len_tests.twr
	./tower -c --interp_lir tests/list.twr tests/length_inefficient.twr tests/stack.twr tests/stack_len_tests.twr
	./tower -c --interp_circuit tests/list.twr tests/length_inefficient.twr tests/stack.twr tests/stack_len_tests.twr
	./tower -c --interp_prim tests/list.twr tests/length_inefficient.twr tests/stack.twr tests/stack_len_tests.twr
	./tower -i tests/list.twr tests/sum.twr tests/stack.twr tests/sum_tests.twr
	./tower -i --inline tests/list.twr tests/sum.twr tests/stack.twr tests/sum_tests.twr
	./tower -c --interp_lir tests/list.twr tests/sum.twr tests/stack.twr tests/sum_tests.twr
	./tower -c --interp_circuit tests/list.twr tests/sum.twr tests/stack.twr tests/sum_tests.twr
	./tower -c --interp_prim tests/list.twr tests/sum.twr tests/stack.twr tests/sum_tests.twr
	./tower -i tests/list.twr tests/contains.twr tests/stack.twr tests/contains_tests.twr
	./tower -i --inline tests/list.twr tests/contains.twr tests/stack.twr tests/contains_tests.twr
	./tower -c --interp_lir tests/list.twr tests/contains.twr tests/stack.twr tests/contains_tests.twr
	./tower -c --interp_circuit tests/list.twr tests/contains.twr tests/stack.twr tests/contains_tests.twr
	./tower -c --interp_prim tests/list.twr tests/contains.twr tests/stack.twr tests/contains_tests.twr
	./tower -i tests/list.twr tests/stack.twr tests/remove.twr tests/remove_tests.twr
	./tower -i --inline tests/list.twr tests/stack.twr tests/remove.twr tests/remove_tests.twr
	./tower -c --interp_lir tests/list.twr tests/stack.twr tests/remove.twr tests/remove_tests.twr
	./tower -c --interp_circuit tests/list.twr tests/stack.twr tests/remove.twr tests/remove_tests.twr
	./tower -c --interp_prim tests/list.twr tests/stack.twr tests/remove.twr tests/remove_tests.twr
	./tower -i tests/list.twr tests/stack.twr tests/find_pos.twr tests/find_pos_tests.twr
	./tower -i --inline tests/list.twr tests/stack.twr tests/find_pos.twr tests/find_pos_tests.twr
	./tower -c --interp_lir tests/list.twr tests/stack.twr tests/find_pos.twr tests/find_pos_tests.twr
	./tower -c --interp_circuit tests/list.twr tests/stack.twr tests/find_pos.twr tests/find_pos_tests.twr
	./tower -c --interp_prim tests/list.twr tests/stack.twr tests/find_pos.twr tests/find_pos_tests.twr
	./tower -i tests/list.twr tests/queue.twr tests/queue_tests.twr
	./tower -i --inline tests/list.twr tests/queue.twr tests/queue_tests.twr
	./tower -c --interp_lir tests/list.twr tests/queue.twr tests/queue_tests.twr
	./tower -c --interp_circuit tests/list.twr tests/queue.twr tests/queue_tests.twr
	./tower -c --interp_prim tests/list.twr tests/queue.twr tests/queue_tests.twr
	./tower -i tests/string.twr tests/string_tests.twr
#	./tower -i --inline tests/string.twr tests/string_tests.twr
#	./tower -c tests/string.twr tests/string_tests.twr
	./tower -i tests/hash_table.twr tests/hash_table_tests.twr
#	./tower -i --inline tests/hash_table.twr tests/hash_table_tests.twr
#	./tower -c tests/hash_table.twr tests/hash_table_tests.twr
	./tower -i tests/word.twr tests/word_tests.twr
	./tower -i --inline tests/word.twr tests/word_tests.twr
	./tower -c --interp_lir tests/word.twr tests/word_tests.twr
	./tower -c --interp_circuit tests/word.twr tests/word_tests.twr
	./tower -c --interp_prim tests/word.twr tests/word_tests.twr
	./tower -i tests/word.twr tests/string_word.twr tests/string_word_tests.twr
	./tower -i --inline tests/word.twr tests/string_word.twr tests/string_word_tests.twr
	./tower -c --interp_lir tests/word.twr tests/string_word.twr tests/string_word_tests.twr
	./tower -c --interp_circuit tests/word.twr tests/string_word.twr tests/string_word_tests.twr
	./tower -c --interp_prim tests/word.twr tests/string_word.twr tests/string_word_tests.twr
	./tower -i tests/word.twr tests/string_word.twr tests/radix_tree.twr tests/radix_tree_tests.twr
	./tower -i --inline tests/word.twr tests/string_word.twr tests/radix_tree.twr tests/radix_tree_tests.twr
# the following take >1 minute each to execute
#	./tower -c tests/word.twr tests/string_word.twr tests/radix_tree.twr tests/radix_tree_tests.twr
#	./tower -c --interp_lir tests/word.twr tests/string_word.twr tests/radix_tree.twr tests/radix_tree_tests.twr
#	./tower -c --interp_circuit tests/word.twr tests/string_word.twr tests/radix_tree.twr tests/radix_tree_tests.twr
#	./tower -c --interp_prim tests/word.twr tests/string_word.twr tests/radix_tree.twr tests/radix_tree_tests.twr
