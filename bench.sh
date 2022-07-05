#!/bin/bash

echo "List length"
echo "Expected complexity: 34n + 32 qubits, 23n + 3 gates"
for i in {1..10}
do
a=$(./tower --no-prim -c -v -b length:$i tests/list.twr tests/length.twr 2>/dev/null | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 34 \* $i + 32)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v -b length:$i tests/list.twr tests/length.twr 2>/dev/null | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 23 \* $i + 3)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i gates\t$c: actual $a, expected: $e"
done

echo "List sum"
echo "Expected complexity: 34n + 40 qubits, 21n + 3 gates"
for i in {1..10}
do
a=$(./tower --no-prim -c -v -b sum:$i tests/list.twr tests/sum.twr 2>/dev/null | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 34 \* $i + 40)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v -b sum:$i tests/list.twr tests/sum.twr 2>/dev/null | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 21 \* $i + 3)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i gates\t$c: actual $a, expected: $e"
done

echo "List find_pos"
echo "Expected complexity: 42n + 31 qubits, 19n + 3 gates"
for i in {1..10}
do
a=$(./tower --no-prim -c -v -b find_pos:$i tests/list.twr tests/find_pos.twr 2>/dev/null | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 42 \* $i + 31)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v -b find_pos:$i tests/list.twr tests/find_pos.twr 2>/dev/null | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 19 \* $i + 3)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i gates\t$c: actual $a, expected: $e"
done

echo "List remove"
echo "Expected complexity: 26n + 56 qubits, 42n + 3 gates"
for i in {1..10}
do
a=$(./tower --no-prim -c -v -b remove:$i tests/list.twr tests/remove.twr 2>/dev/null | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 26 \* $i + 56)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v -b remove:$i tests/list.twr tests/remove.twr 2>/dev/null | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 42 \* $i + 3)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i gates\t$c: actual $a, expected: $e"
done

echo "Stack push_front"
echo "Expected complexity: 40 qubits, 4 gates"
a=$(./tower --no-prim -c -v tests/list.twr tests/stack.twr 2>/dev/null | grep -A2 push_front | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 40)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v tests/list.twr tests/stack.twr 2>/dev/null | grep -A2 push_front | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 4)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i gates\t$c: actual $a, expected: $e"

echo "Stack pop_front"
echo "Expected complexity: 48 qubits, 4 gates"
a=$(./tower --no-prim -c -v tests/list.twr tests/stack.twr 2>/dev/null | grep -A2 pop_front | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 48)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v tests/list.twr tests/stack.twr 2>/dev/null | grep -A2 pop_front | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 4)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i gates\t$c: actual $a, expected: $e"

echo "Queue push_back"
echo "Expected complexity: 34n + 32 qubits, 24n gates"
for i in {1..10}
do
a=$(./tower --no-prim -c -v -b push_back:$i tests/list.twr tests/queue.twr 2>/dev/null | grep -A2 push_back | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 34 \* $i + 32)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v -b push_back:$i tests/list.twr tests/queue.twr 2>/dev/null | grep -A2 push_back | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 24 \* $i)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i gates\t$c: actual $a, expected: $e"
done

echo "Queue pop_front"
echo "Expected complexity: 48 qubits, 4 gates"
a=$(./tower --no-prim -c -v tests/list.twr tests/queue.twr 2>/dev/null | grep -A2 pop_front | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 48)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v tests/list.twr tests/queue.twr 2>/dev/null | grep -A2 pop_front | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 4)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i gates\t$c: actual $a, expected: $e"

echo "String is_empty"
echo "Expected complexity: 25 qubits, 3 gates"
a=$(./tower --no-prim -c -v tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 is_empty | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 25)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 is_empty | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 3)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i gates\t$c: actual $a, expected: $e"

echo "String length"
echo "Expected complexity: 24 qubits, 1 gate"
a=$(./tower --no-prim -c -v tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 length | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 24)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 length | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 1)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i gates\t$c: actual $a, expected: $e"

echo "String get_prefix"
echo "Expected complexity: 11k qubits, 52 gates"
for i in {4..12}
do
a=$(./tower --no-prim -c -v --word_size $i tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 get_prefix | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 11 \* $i)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v --word_size $i tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 get_prefix | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 52)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i gates\t$c: actual $a, expected: $e"
done

echo "String get_substring"
echo "Expected complexity: 12k qubits, 54 gates"
for i in {4..12}
do
a=$(./tower --no-prim -c -v --word_size $i tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 get_substring | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 12 \* $i)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v --word_size $i tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 get_substring | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 54)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i gates\t$c: actual $a, expected: $e"
done

echo "String get"
echo "Expected complexity: 6k + 1 qubits, 19 gates"
for i in {4..12}
do
a=$(./tower --no-prim -c -v --word_size $i tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 \'get\' | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 6 \* $i + 1)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v --word_size $i tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 \'get\' | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 19)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i gates\t$c: actual $a, expected: $e"
done

echo "String is_prefix"
echo "Expected complexity: k^2 + 11k qubits, 98k + 3 gates"
for i in {4..12}
do
a=$(./tower --no-prim -c -v --word_size $i -b is_prefix:$i tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 \'is_prefix\' | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr $i \* $i + 11 \* $i)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v --word_size $i -b is_prefix:$i tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 \'is_prefix\' | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 98 \* $i + 3)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i gates\t$c: actual $a, expected: $e"
done

echo "String num_matching"
echo "Expected complexity: k^2 + 13k + 4 qubits, 110k + 127 gates"
for i in {4..12}
do
a=$(./tower --no-prim -c -v --word_size $i -b num_matching:$i tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 \'num_matching\' | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr $i \* $i + 13 \* $i + 4)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v --word_size $i -b num_matching:$i tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 \'num_matching\' | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 110 \* $i + 127)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i gates\t$c: actual $a, expected: $e"
done

echo "String equal"
echo "Expected complexity: 6k + 3 qubits, 5 gates"
for i in {4..12}
do
a=$(./tower --no-prim -c -v --word_size $i tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 \'equal\' | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 6 \* $i + 3)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v --word_size $i tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 \'equal\' | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 5)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i gates\t$c: actual $a, expected: $e"
done

echo "String concat"
echo "Expected complexity: 11k qubits, 8 gates"
for i in {4..12}
do
a=$(./tower --no-prim -c -v --word_size $i tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 \'concat\' | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 11 \* $i)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v --word_size $i tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 \'concat\' | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 8)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i gates\t$c: actual $a, expected: $e"
done

echo "String compare"
echo "Expected complexity: 5k^2 + 12k qubits, 108k + 3 gates"
for i in {4..12}
do
a=$(./tower --no-prim -c -v --word_size $i -b compare:$i tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 \'compare\' | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 5 \* $i \* $i + 12 \* $i)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v --word_size $i -b compare:$i tests/word.twr tests/string_word.twr 2>/dev/null | grep -A2 \'compare\' | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 108 \* $i + 3)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i gates\t$c: actual $a, expected: $e"
done

echo "Set (radix tree) insert"
echo "Expected complexity: 13k^2 + 21k + 9 qubits, 1440k^2 + 5056k gates"
for i in {4..12}
do
a=$(./tower --no-prim -c -v --word_size $i -b insert:$i tests/word.twr tests/string_word.twr tests/radix_tree.twr 2>/dev/null | grep -A2 \'insert\' | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 13 \* $i \* $i + 21 \* $i + 9)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v --word_size $i -b insert:$i tests/word.twr tests/string_word.twr tests/radix_tree.twr 2>/dev/null | grep -A2 \'insert\' | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 1440 \* $i \* $i + 5056 \* $i)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i gates\t$c: actual $a, expected: $e"
done

echo "Set (radix tree) contains"
echo "Expected complexity: 17k^2 + 18k + 2 qubits, 784k^2 + 1612k + 1 gates"
for i in {4..12}
do
a=$(./tower --no-prim -c -v --word_size $i -b contains:$i tests/word.twr tests/string_word.twr tests/radix_tree.twr 2>/dev/null | grep -A2 \'contains\' | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 17 \* $i \* $i + 18 \* $i + 2)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v --word_size $i -b contains:$i tests/word.twr tests/string_word.twr tests/radix_tree.twr 2>/dev/null | grep -A2 \'contains\' | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 784 \* $i \* $i + 1612 \* $i + 1)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "k=$i gates\t$c: actual $a, expected: $e"
done

echo "Set (hash table) insert"
echo "Expected complexity: 52n + 72 qubits, 68n + 15 gates"
for i in {4..12}
do
a=$(./tower --no-prim -c -v -b insert:$i tests/hash_table.twr 2>/dev/null | grep -A2 \'insert\' | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 52 \* $i + 72)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v -b insert:$i tests/hash_table.twr 2>/dev/null | grep -A2 \'insert\' | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 68 \* $i + 15)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i gates\t$c: actual $a, expected: $e"
done

echo "Set (hash table) contains"
echo "Expected complexity: 52n + 81 qubits, 136n + 39 gates"
for i in {4..12}
do
a=$(./tower --no-prim -c -v -b contains:$i tests/hash_table.twr 2>/dev/null | grep -A2 \'contains\' | grep 'total qubits' | awk '{print $NF}' | tr -d '\n')
e=$(expr 52 \* $i + 81)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i qubits\t$c: actual $a, expected: $e"
a=$(./tower --no-prim -c -v -b contains:$i tests/hash_table.twr 2>/dev/null | grep -A2 \'contains\' | grep 'gates' | awk '{print $NF}' | tr -d '\n')
e=$(expr 136 \* $i + 39)
c=$([ $a == $e ] && echo "PASS" || echo "FAIL")
echo -e "n=$i gates\t$c: actual $a, expected: $e"
done
