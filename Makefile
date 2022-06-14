all:
	rm -f ./tower; dune build && ln -s ./_build/default/src/main.exe ./tower

clean:
	dune clean && rm -f ./tower

check:
	./tower tests/list.twr tests/len.twr tests/stack.twr tests/stack_len_tests.twr
	./tower tests/list.twr tests/len_inefficient.twr tests/stack.twr tests/stack_len_tests.twr
	./tower tests/list.twr tests/sum.twr tests/stack.twr tests/sum_tests.twr
	./tower tests/list.twr tests/contains.twr tests/stack.twr tests/contains_tests.twr
	./tower tests/list.twr tests/stack.twr tests/remove.twr tests/remove_tests.twr
	./tower tests/list.twr tests/stack.twr tests/find_pos.twr tests/find_pos_tests.twr
	./tower tests/list.twr tests/queue.twr tests/queue_tests.twr
	./tower tests/string.twr tests/string_tests.twr
	./tower tests/hash_table.twr tests/hash_table_tests.twr
	./tower tests/word.twr tests/word_tests.twr
	./tower tests/word.twr tests/string_word.twr tests/string_word_tests.twr
	./tower tests/word.twr tests/string_word.twr tests/radix_tree.twr tests/radix_tree_tests.twr
