begin_version
3
end_version
begin_metric
0
end_metric
10
begin_variable
var0
-1
2
Atom at(apn1, apt1)
NegatedAtom at(apn1, apt1)
end_variable
begin_variable
var1
-1
2
Atom at(apn1, apt2)
NegatedAtom at(apn1, apt2)
end_variable
begin_variable
var2
-1
2
Atom at(apn1, pos1)
NegatedAtom at(apn1, pos1)
end_variable
begin_variable
var3
-1
2
Atom at(apn1, pos2)
NegatedAtom at(apn1, pos2)
end_variable
begin_variable
var4
-1
5
Atom at(obj11, apt1)
Atom at(obj11, apt2)
Atom at(obj11, pos1)
Atom at(obj11, pos2)
Atom in(obj11, apn1)
end_variable
begin_variable
var5
-1
5
Atom at(obj12, apt1)
Atom at(obj12, apt2)
Atom at(obj12, pos1)
Atom at(obj12, pos2)
Atom in(obj12, apn1)
end_variable
begin_variable
var6
-1
5
Atom at(obj13, apt1)
Atom at(obj13, apt2)
Atom at(obj13, pos1)
Atom at(obj13, pos2)
Atom in(obj13, apn1)
end_variable
begin_variable
var7
-1
5
Atom at(obj21, apt1)
Atom at(obj21, apt2)
Atom at(obj21, pos1)
Atom at(obj21, pos2)
Atom in(obj21, apn1)
end_variable
begin_variable
var8
-1
5
Atom at(obj22, apt1)
Atom at(obj22, apt2)
Atom at(obj22, pos1)
Atom at(obj22, pos2)
Atom in(obj22, apn1)
end_variable
begin_variable
var9
-1
5
Atom at(obj23, apt1)
Atom at(obj23, apt2)
Atom at(obj23, pos1)
Atom at(obj23, pos2)
Atom in(obj23, apn1)
end_variable
6
begin_mutex_group
5
4 0
4 1
4 2
4 3
4 4
end_mutex_group
begin_mutex_group
5
5 0
5 1
5 2
5 3
5 4
end_mutex_group
begin_mutex_group
5
6 0
6 1
6 2
6 3
6 4
end_mutex_group
begin_mutex_group
5
7 0
7 1
7 2
7 3
7 4
end_mutex_group
begin_mutex_group
5
8 0
8 1
8 2
8 3
8 4
end_mutex_group
begin_mutex_group
5
9 0
9 1
9 2
9 3
9 4
end_mutex_group
begin_state
0
1
0
0
2
2
2
3
3
3
end_state
begin_goal
5
4 3
5 3
6 1
8 0
9 1
end_goal
86
begin_operator
drive-truck apn1 apt1 apt2 cit2
0
2
0 0 0 1
0 1 -1 0
1
end_operator
begin_operator
drive-truck apn1 apt1 pos1 cit2
0
2
0 0 0 1
0 2 -1 0
1
end_operator
begin_operator
drive-truck apn1 apt1 pos2 cit2
0
2
0 0 0 1
0 3 -1 0
1
end_operator
begin_operator
drive-truck apn1 apt2 apt1 cit2
0
2
0 0 -1 0
0 1 0 1
1
end_operator
begin_operator
drive-truck apn1 apt2 pos1 cit2
0
2
0 1 0 1
0 2 -1 0
1
end_operator
begin_operator
drive-truck apn1 apt2 pos2 cit2
0
2
0 1 0 1
0 3 -1 0
1
end_operator
begin_operator
drive-truck apn1 pos1 apt1 cit2
0
2
0 0 -1 0
0 2 0 1
1
end_operator
begin_operator
drive-truck apn1 pos1 apt2 cit2
0
2
0 1 -1 0
0 2 0 1
1
end_operator
begin_operator
drive-truck apn1 pos1 pos2 cit2
0
2
0 2 0 1
0 3 -1 0
1
end_operator
begin_operator
drive-truck apn1 pos2 apt1 cit2
0
2
0 0 -1 0
0 3 0 1
1
end_operator
begin_operator
drive-truck apn1 pos2 apt2 cit2
0
2
0 1 -1 0
0 3 0 1
1
end_operator
begin_operator
drive-truck apn1 pos2 pos1 cit2
0
2
0 2 -1 0
0 3 0 1
1
end_operator
begin_operator
fly-airplane apn1 apt1 apt2
0
2
0 0 0 1
0 1 -1 0
1
end_operator
begin_operator
fly-airplane apn1 apt2 apt1
0
2
0 0 -1 0
0 1 0 1
1
end_operator
begin_operator
load-airplane apn1 obj11 apt1
1
0 0
1
0 4 0 4
1
end_operator
begin_operator
load-airplane apn1 obj11 apt2
1
1 0
1
0 4 1 4
1
end_operator
begin_operator
load-airplane apn1 obj12 apt1
1
0 0
1
0 5 0 4
1
end_operator
begin_operator
load-airplane apn1 obj12 apt2
1
1 0
1
0 5 1 4
1
end_operator
begin_operator
load-airplane apn1 obj13 apt1
1
0 0
1
0 6 0 4
1
end_operator
begin_operator
load-airplane apn1 obj13 apt2
1
1 0
1
0 6 1 4
1
end_operator
begin_operator
load-airplane apn1 obj21 apt1
1
0 0
1
0 7 0 4
1
end_operator
begin_operator
load-airplane apn1 obj21 apt2
1
1 0
1
0 7 1 4
1
end_operator
begin_operator
load-airplane apn1 obj22 apt1
1
0 0
1
0 8 0 4
1
end_operator
begin_operator
load-airplane apn1 obj22 apt2
1
1 0
1
0 8 1 4
1
end_operator
begin_operator
load-airplane apn1 obj23 apt1
1
0 0
1
0 9 0 4
1
end_operator
begin_operator
load-airplane apn1 obj23 apt2
1
1 0
1
0 9 1 4
1
end_operator
begin_operator
load-truck apn1 obj11 apt1
1
0 0
1
0 4 0 4
1
end_operator
begin_operator
load-truck apn1 obj11 apt2
1
1 0
1
0 4 1 4
1
end_operator
begin_operator
load-truck apn1 obj11 pos1
1
2 0
1
0 4 2 4
1
end_operator
begin_operator
load-truck apn1 obj11 pos2
1
3 0
1
0 4 3 4
1
end_operator
begin_operator
load-truck apn1 obj12 apt1
1
0 0
1
0 5 0 4
1
end_operator
begin_operator
load-truck apn1 obj12 apt2
1
1 0
1
0 5 1 4
1
end_operator
begin_operator
load-truck apn1 obj12 pos1
1
2 0
1
0 5 2 4
1
end_operator
begin_operator
load-truck apn1 obj12 pos2
1
3 0
1
0 5 3 4
1
end_operator
begin_operator
load-truck apn1 obj13 apt1
1
0 0
1
0 6 0 4
1
end_operator
begin_operator
load-truck apn1 obj13 apt2
1
1 0
1
0 6 1 4
1
end_operator
begin_operator
load-truck apn1 obj13 pos1
1
2 0
1
0 6 2 4
1
end_operator
begin_operator
load-truck apn1 obj13 pos2
1
3 0
1
0 6 3 4
1
end_operator
begin_operator
load-truck apn1 obj21 apt1
1
0 0
1
0 7 0 4
1
end_operator
begin_operator
load-truck apn1 obj21 apt2
1
1 0
1
0 7 1 4
1
end_operator
begin_operator
load-truck apn1 obj21 pos1
1
2 0
1
0 7 2 4
1
end_operator
begin_operator
load-truck apn1 obj21 pos2
1
3 0
1
0 7 3 4
1
end_operator
begin_operator
load-truck apn1 obj22 apt1
1
0 0
1
0 8 0 4
1
end_operator
begin_operator
load-truck apn1 obj22 apt2
1
1 0
1
0 8 1 4
1
end_operator
begin_operator
load-truck apn1 obj22 pos1
1
2 0
1
0 8 2 4
1
end_operator
begin_operator
load-truck apn1 obj22 pos2
1
3 0
1
0 8 3 4
1
end_operator
begin_operator
load-truck apn1 obj23 apt1
1
0 0
1
0 9 0 4
1
end_operator
begin_operator
load-truck apn1 obj23 apt2
1
1 0
1
0 9 1 4
1
end_operator
begin_operator
load-truck apn1 obj23 pos1
1
2 0
1
0 9 2 4
1
end_operator
begin_operator
load-truck apn1 obj23 pos2
1
3 0
1
0 9 3 4
1
end_operator
begin_operator
unload-airplane apn1 obj11 apt1
1
0 0
1
0 4 4 0
1
end_operator
begin_operator
unload-airplane apn1 obj11 apt2
1
1 0
1
0 4 4 1
1
end_operator
begin_operator
unload-airplane apn1 obj12 apt1
1
0 0
1
0 5 4 0
1
end_operator
begin_operator
unload-airplane apn1 obj12 apt2
1
1 0
1
0 5 4 1
1
end_operator
begin_operator
unload-airplane apn1 obj13 apt1
1
0 0
1
0 6 4 0
1
end_operator
begin_operator
unload-airplane apn1 obj13 apt2
1
1 0
1
0 6 4 1
1
end_operator
begin_operator
unload-airplane apn1 obj21 apt1
1
0 0
1
0 7 4 0
1
end_operator
begin_operator
unload-airplane apn1 obj21 apt2
1
1 0
1
0 7 4 1
1
end_operator
begin_operator
unload-airplane apn1 obj22 apt1
1
0 0
1
0 8 4 0
1
end_operator
begin_operator
unload-airplane apn1 obj22 apt2
1
1 0
1
0 8 4 1
1
end_operator
begin_operator
unload-airplane apn1 obj23 apt1
1
0 0
1
0 9 4 0
1
end_operator
begin_operator
unload-airplane apn1 obj23 apt2
1
1 0
1
0 9 4 1
1
end_operator
begin_operator
unload-truck apn1 obj11 apt1
1
0 0
1
0 4 4 0
1
end_operator
begin_operator
unload-truck apn1 obj11 apt2
1
1 0
1
0 4 4 1
1
end_operator
begin_operator
unload-truck apn1 obj11 pos1
1
2 0
1
0 4 4 2
1
end_operator
begin_operator
unload-truck apn1 obj11 pos2
1
3 0
1
0 4 4 3
1
end_operator
begin_operator
unload-truck apn1 obj12 apt1
1
0 0
1
0 5 4 0
1
end_operator
begin_operator
unload-truck apn1 obj12 apt2
1
1 0
1
0 5 4 1
1
end_operator
begin_operator
unload-truck apn1 obj12 pos1
1
2 0
1
0 5 4 2
1
end_operator
begin_operator
unload-truck apn1 obj12 pos2
1
3 0
1
0 5 4 3
1
end_operator
begin_operator
unload-truck apn1 obj13 apt1
1
0 0
1
0 6 4 0
1
end_operator
begin_operator
unload-truck apn1 obj13 apt2
1
1 0
1
0 6 4 1
1
end_operator
begin_operator
unload-truck apn1 obj13 pos1
1
2 0
1
0 6 4 2
1
end_operator
begin_operator
unload-truck apn1 obj13 pos2
1
3 0
1
0 6 4 3
1
end_operator
begin_operator
unload-truck apn1 obj21 apt1
1
0 0
1
0 7 4 0
1
end_operator
begin_operator
unload-truck apn1 obj21 apt2
1
1 0
1
0 7 4 1
1
end_operator
begin_operator
unload-truck apn1 obj21 pos1
1
2 0
1
0 7 4 2
1
end_operator
begin_operator
unload-truck apn1 obj21 pos2
1
3 0
1
0 7 4 3
1
end_operator
begin_operator
unload-truck apn1 obj22 apt1
1
0 0
1
0 8 4 0
1
end_operator
begin_operator
unload-truck apn1 obj22 apt2
1
1 0
1
0 8 4 1
1
end_operator
begin_operator
unload-truck apn1 obj22 pos1
1
2 0
1
0 8 4 2
1
end_operator
begin_operator
unload-truck apn1 obj22 pos2
1
3 0
1
0 8 4 3
1
end_operator
begin_operator
unload-truck apn1 obj23 apt1
1
0 0
1
0 9 4 0
1
end_operator
begin_operator
unload-truck apn1 obj23 apt2
1
1 0
1
0 9 4 1
1
end_operator
begin_operator
unload-truck apn1 obj23 pos1
1
2 0
1
0 9 4 2
1
end_operator
begin_operator
unload-truck apn1 obj23 pos2
1
3 0
1
0 9 4 3
1
end_operator
0
