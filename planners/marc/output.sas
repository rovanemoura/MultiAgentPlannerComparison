begin_version
3
end_version
begin_metric
0
end_metric
9
begin_variable
var0
-1
3
Atom energy(node1, low)
Atom energy(node1, normal)
Atom energy(node1, zero)
end_variable
begin_variable
var1
-1
2
Atom has-data(base, node1)
NegatedAtom has-data(base, node1)
end_variable
begin_variable
var2
-1
2
Atom has-data(node1, node1)
NegatedAtom has-data(node1, node1)
end_variable
begin_variable
var3
-1
2
Atom is-message-at(msg1-1, base)
Atom not-message-at(msg1-1, base)
end_variable
begin_variable
var4
-1
2
Atom is-message-at(msg1-1, node1)
NegatedAtom is-message-at(msg1-1, node1)
end_variable
begin_variable
var5
-1
2
Atom message-data(msg1-1, node1)
Atom not-message-data(msg1-1, node1)
end_variable
begin_variable
var6
-1
2
Atom not-message-at(msg1-1, node1)
NegatedAtom not-message-at(msg1-1, node1)
end_variable
begin_variable
var7
-1
2
Atom not-sending(node1, base, msg1-1)
Atom sending(node1, base, msg1-1)
end_variable
begin_variable
var8
-1
2
Atom not-sending(node1, node1, msg1-1)
Atom sending(node1, node1, msg1-1)
end_variable
12
begin_mutex_group
3
0 0
0 1
0 2
end_mutex_group
begin_mutex_group
2
3 0
3 1
end_mutex_group
begin_mutex_group
2
3 0
3 1
end_mutex_group
begin_mutex_group
2
5 0
5 1
end_mutex_group
begin_mutex_group
2
5 0
5 1
end_mutex_group
begin_mutex_group
2
5 0
5 1
end_mutex_group
begin_mutex_group
2
7 0
7 1
end_mutex_group
begin_mutex_group
2
7 0
7 1
end_mutex_group
begin_mutex_group
2
7 0
7 1
end_mutex_group
begin_mutex_group
2
8 0
8 1
end_mutex_group
begin_mutex_group
2
8 0
8 1
end_mutex_group
begin_mutex_group
2
8 0
8 1
end_mutex_group
begin_state
1
1
1
1
0
1
0
0
0
end_state
begin_goal
0
end_goal
11
begin_operator
add-to-message node1 node1 msg1-1
1
4 0
2
0 2 0 1
0 5 1 0
0
end_operator
begin_operator
generate-data node1 low zero
0
2
0 0 0 2
0 2 -1 0
0
end_operator
begin_operator
generate-data node1 normal low
0
2
0 0 1 0
0 2 -1 0
0
end_operator
begin_operator
get-data-from-message base node1 msg1-1
1
3 0
2
0 1 -1 0
0 5 0 1
0
end_operator
begin_operator
get-data-from-message node1 node1 msg1-1
1
4 0
2
0 2 -1 0
0 5 0 1
0
end_operator
begin_operator
receive-message base node1 msg1-1
0
2
0 3 1 0
0 7 1 0
0
end_operator
begin_operator
receive-message node1 node1 msg1-1
0
3
0 4 -1 0
0 6 0 1
0 8 1 0
0
end_operator
begin_operator
send-message node1 base msg1-1 low zero
1
3 1
4
0 0 0 2
0 4 0 1
0 6 -1 0
0 7 0 1
0
end_operator
begin_operator
send-message node1 base msg1-1 normal low
1
3 1
4
0 0 1 0
0 4 0 1
0 6 -1 0
0 7 0 1
0
end_operator
begin_operator
send-message node1 node1 msg1-1 low zero
1
6 0
3
0 0 0 2
0 4 0 1
0 8 0 1
0
end_operator
begin_operator
send-message node1 node1 msg1-1 normal low
1
6 0
3
0 0 1 0
0 4 0 1
0 8 0 1
0
end_operator
0
