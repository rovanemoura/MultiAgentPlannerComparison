Dispatcher selected state size 1.
This is a unit task.
Simplifying transitions... done!
Initializing Exploration...
Reading invariants from file...
done
Generating landmarks using the RPG/SAS+ approach
approx. reasonable orders
approx. obedient reasonable orders
Removed 0 reasonable or obedient reasonable orders
Landmarks generation time: 0.00399295s
Discovered 51 landmarks, of which 1 are disjunctive and 0 are conjunctive 
65 edges
Initializing LAMA-FF Synergy Object
Initializing landmarks count heuristic...
25 initial landmarks, 11 goal landmarks
insert bound
Conducting lazy best first search, (real) bound = 2147483647
Best heuristic value: 26/29 [g=0, 1 evaluated, 0 expanded, t=0s]
Best heuristic value: 25/28 [g=1, 2 evaluated, 1 expanded, t=0s]
Best heuristic value: 25/27 [g=2, 5 evaluated, 4 expanded, t=0s]
Best heuristic value: 24/26 [g=3, 10 evaluated, 8 expanded, t=0s]
Best heuristic value: 23/25 [g=4, 13 evaluated, 11 expanded, t=0s]
Best heuristic value: 22/24 [g=5, 21 evaluated, 18 expanded, t=0s]
Best heuristic value: 21/23 [g=6, 29 evaluated, 25 expanded, t=0s]
Best heuristic value: 20/22 [g=8, 38 evaluated, 33 expanded, t=0.01s]
Best heuristic value: 19/21 [g=9, 39 evaluated, 34 expanded, t=0.01s]
Best heuristic value: 18/20 [g=10, 40 evaluated, 35 expanded, t=0.01s]
Best heuristic value: 17/19 [g=11, 41 evaluated, 36 expanded, t=0.01s]
Best heuristic value: 16/18 [g=13, 49 evaluated, 42 expanded, t=0.01s]
Best heuristic value: 15/17 [g=14, 51 evaluated, 43 expanded, t=0.01s]
Best heuristic value: 14/16 [g=16, 61 evaluated, 51 expanded, t=0.01s]
Best heuristic value: 13/15 [g=17, 66 evaluated, 55 expanded, t=0.01s]
Best heuristic value: 13/14 [g=20, 73 evaluated, 61 expanded, t=0.01s]
Best heuristic value: 13/13 [g=21, 77 evaluated, 64 expanded, t=0.01s]
Best heuristic value: 12/12 [g=22, 84 evaluated, 70 expanded, t=0.01s]
Best heuristic value: 11/11 [g=25, 91 evaluated, 76 expanded, t=0.01s]
Best heuristic value: 10/10 [g=26, 93 evaluated, 77 expanded, t=0.01s]
Best heuristic value: 9/9 [g=29, 99 evaluated, 82 expanded, t=0.01s]
Best heuristic value: 8/8 [g=31, 103 evaluated, 85 expanded, t=0.01s]
Best heuristic value: 7/7 [g=34, 107 evaluated, 88 expanded, t=0.01s]
Best heuristic value: 6/6 [g=35, 109 evaluated, 89 expanded, t=0.01s]
Best heuristic value: 5/5 [g=36, 111 evaluated, 90 expanded, t=0.01s]
Best heuristic value: 4/4 [g=37, 112 evaluated, 91 expanded, t=0.01s]
Best heuristic value: 3/3 [g=38, 114 evaluated, 92 expanded, t=0.01s]
Best heuristic value: 2/2 [g=39, 115 evaluated, 93 expanded, t=0.01s]
Best heuristic value: 1/1 [g=40, 116 evaluated, 94 expanded, t=0.01s]
Solution found!
Actual search time: 0.01s [t=0.01s]
calibrate rover1 camera1 objective3 waypoint0 (1)
calibrate rover1 camera0 objective2 waypoint0 (1)
take_image rover1 waypoint0 objective3 camera0 low_res (1)
c_i_d rover1 general objective3 low_res waypoint0 waypoint1 (1)
sample_soil rover1 rover1store waypoint0 (1)
communicate_soil_data rover1 general waypoint0 waypoint0 waypoint1 (1)
take_image rover1 waypoint0 objective2 camera1 colour (1)
c_i_d rover1 general objective2 colour waypoint0 waypoint1 (1)
calibrate rover1 camera1 objective3 waypoint0 (1)
take_image rover1 waypoint0 objective3 camera1 colour (1)
c_i_d rover1 general objective3 colour waypoint0 waypoint1 (1)
sample_rock---rover0---rover0---anon-rover0-18---waypoint4  (1)
drop---rover0---rover0---anon-rover0-18  (1)
communicate_rock_data---rover0---rover0---general---waypoint4---waypoint4---waypoint1  (1)
sample_soil---rover0---rover0---anon-rover0-18---waypoint4  (1)
drop---rover0---rover0---anon-rover0-18  (1)
communicate_soil_data---rover0---rover0---general---waypoint4---waypoint4---waypoint1  (1)
navigate---rover0---rover0---waypoint4---waypoint6  (1)
sample_soil---rover0---rover0---anon-rover0-18---waypoint6  (1)
navigate---rover0---rover0---waypoint6---waypoint4  (1)
drop---rover0---rover0---anon-rover0-18  (1)
communicate_soil_data---rover0---rover0---general---waypoint6---waypoint4---waypoint1  (1)
navigate---rover0---rover0---waypoint4---waypoint0  (1)
sample_rock---rover0---rover0---anon-rover0-18---waypoint0  (1)
navigate---rover0---rover0---waypoint0---waypoint4  (1)
drop---rover0---rover0---anon-rover0-18  (1)
navigate---rover0---rover0---waypoint4---waypoint3  (1)
sample_soil---rover0---rover0---anon-rover0-18---waypoint3  (1)
drop---rover0---rover0---anon-rover0-18  (1)
navigate---rover0---rover0---waypoint3---waypoint4  (1)
communicate_soil_data---rover0---rover0---general---waypoint3---waypoint4---waypoint1  (1)
navigate---rover0---rover0---waypoint4---waypoint3  (1)
sample_rock---rover0---rover0---anon-rover0-18---waypoint3  (1)
navigate---rover0---rover0---waypoint3---waypoint4  (1)
drop---rover0---rover0---anon-rover0-18  (1)
communicate_rock_data---rover0---rover0---general---waypoint3---waypoint4---waypoint1  (1)
navigate---rover0---rover0---waypoint4---waypoint1  (1)
sample_rock---rover0---rover0---anon-rover0-18---waypoint1  (1)
navigate---rover0---rover0---waypoint1---waypoint5  (1)
communicate_rock_data---rover0---rover0---general---waypoint1---waypoint5---waypoint1  (1)
communicate_rock_data---rover0---rover0---general---waypoint0---waypoint5---waypoint1  (1)
Plan length: 41 step(s).
Plan cost: 41
Initial state h value: 26/29.
Expanded 95 state(s).
Reopened 0 state(s).
Evaluated 117 state(s).
Evaluations: 234
Generated 1535 state(s).
Search time: 0.01s
Total time: 0.01s
Peak memory: 2664 KB
