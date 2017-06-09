INFO     Running translator.
INFO     translator inputs: ['/home/rovane/planners/benchmarks/unfactored/floods/p05/merged-obfuscated-domain.pddl', '/home/rovane/planners/benchmarks/unfactored/floods/p05/merged-obfuscated-problem.pddl']
INFO     translator arguments: []
Parsing...
Parsing: [0.010s CPU, 0.008s wall-clock]
Normalizing task... [0.000s CPU, 0.001s wall-clock]
Instantiating...
Generating Datalog program... [0.000s CPU, 0.002s wall-clock]
Normalizing Datalog program...
Normalizing Datalog program: [0.010s CPU, 0.010s wall-clock]
Preparing model... [0.000s CPU, 0.007s wall-clock]
Generated 233 rules.
Computing model... [0.100s CPU, 0.095s wall-clock]
4623 relevant atoms
2234 auxiliary atoms
6857 final queue length
10749 total queue pushes
Completing instantiation... [0.140s CPU, 0.142s wall-clock]
Instantiating: [0.250s CPU, 0.259s wall-clock]
Computing fact groups...
Finding invariants...
61 initial candidates
Finding invariants: [0.030s CPU, 0.031s wall-clock]
Checking invariant weight... [0.000s CPU, 0.001s wall-clock]
Instantiating groups... [0.010s CPU, 0.006s wall-clock]
Collecting mutex groups... [0.000s CPU, 0.000s wall-clock]
Choosing groups...
329 uncovered facts
Choosing groups: [0.010s CPU, 0.001s wall-clock]
Building translation key... [0.000s CPU, 0.002s wall-clock]
Computing fact groups: [0.050s CPU, 0.048s wall-clock]
Building STRIPS to SAS dictionary... [0.000s CPU, 0.001s wall-clock]
Building dictionary for full mutex groups... [0.000s CPU, 0.001s wall-clock]
Building mutex information...
Building mutex information: [0.000s CPU, 0.001s wall-clock]
Translating task...
Processing axioms...
Simplifying axioms... [0.000s CPU, 0.000s wall-clock]
Processing axioms: [0.010s CPU, 0.006s wall-clock]
Translating task: [0.140s CPU, 0.142s wall-clock]
2246 effect conditions simplified
0 implied preconditions added
Detecting unreachable propositions...
0 operators removed
28 propositions removed
Detecting unreachable propositions: [0.040s CPU, 0.030s wall-clock]
Translator variables: 336
Translator derived variables: 0
Translator facts: 1093
Translator goal facts: 21
Translator mutex groups: 32
Translator total mutex groups size: 485
Translator operators: 3449
Translator axioms: 0
Translator task size: 17470
Translator peak memory: 44656 KB
Writing output... [0.040s CPU, 0.030s wall-clock]
Done! [0.530s CPU, 0.529s wall-clock]
INFO     Running preprocessor.
INFO     preprocessor input: output.sas
INFO     preprocessor arguments: []
Building causal graph...
The causal graph is not acyclic.
215 variables of 336 necessary
0 of 32 mutex groups necessary.
3449 of 3449 operators necessary.
0 of 0 axiom rules necessary.
Building domain transition graphs...
solveable in poly time 0
Building successor generator...
Preprocessor facts: 851
Preprocessor derived variables: 0
Preprocessor task size: 15008
Writing output...
done
INFO     Running search.
INFO     search input: output
INFO     search executable: /home/rovane/planners/cmap/planning/fd/src/search/downward-release
INFO     search arguments: ['--heuristic', 'hlm,hff=lm_ff_syn(lm_rhw(reasonable_orders=true,lm_cost_type=1,cost_type=1))', '--search', 'lazy_greedy(hff,hlm,preferred=[hff,hlm])', '--internal-plan-file', '/home/rovane/planners/benchmarks/unfactored/floods/p05/result/plan-LAMA-UNIT-COST.lisp']
reading input... [t=0s]
Simplifying transitions... done!
done reading input! [t=0.05s]
building causal graph...done! [t=0.05s]
packing state variables...done! [t=0.05s]
Variables: 215
Facts: 851
Bytes per state: 36
done initalizing global data [t=0.05s]
Initializing Exploration...
Generating landmarks using the RPG/SAS+ approach
approx. reasonable orders
approx. obedient reasonable orders
Removed 0 reasonable or obedient reasonable orders
Landmarks generation time: 0.0197386s
Discovered 42 landmarks, of which 0 are disjunctive and 0 are conjunctive 
21 edges
Initializing LAMA-FF Synergy Object
Initializing landmarks count heuristic...
21 initial landmarks, 21 goal landmarks
Conducting lazy best first search, (real) bound = 2147483647
Best heuristic value: 97 [g=0, 1 evaluated, 0 expanded, t=0.07s, 9412 KB]
Best heuristic value: 96 [g=1, 2 evaluated, 1 expanded, t=0.07s, 9412 KB]
Best heuristic value: 95 [g=2, 3 evaluated, 2 expanded, t=0.07s, 9412 KB]
Best heuristic value: 93 [g=3, 4 evaluated, 3 expanded, t=0.07s, 9412 KB]
Best heuristic value: 92 [g=4, 6 evaluated, 5 expanded, t=0.07s, 9412 KB]
Best heuristic value: 91 [g=7, 9 evaluated, 8 expanded, t=0.07s, 9412 KB]
Best heuristic value: 90 [g=8, 10 evaluated, 9 expanded, t=0.07s, 9412 KB]
Best heuristic value: 89 [g=9, 11 evaluated, 10 expanded, t=0.07s, 9412 KB]
Best heuristic value: 88 [g=10, 12 evaluated, 11 expanded, t=0.07s, 9412 KB]
Best heuristic value: 87 [g=11, 14 evaluated, 13 expanded, t=0.07s, 9412 KB]
Best heuristic value: 86 [g=13, 16 evaluated, 15 expanded, t=0.07s, 9412 KB]
Best heuristic value: 85 [g=14, 17 evaluated, 16 expanded, t=0.07s, 9412 KB]
Best heuristic value: 84 [g=15, 18 evaluated, 17 expanded, t=0.07s, 9412 KB]
Best heuristic value: 83 [g=17, 20 evaluated, 19 expanded, t=0.07s, 9412 KB]
Best heuristic value: 82 [g=18, 21 evaluated, 20 expanded, t=0.07s, 9412 KB]
Best heuristic value: 81 [g=19, 22 evaluated, 21 expanded, t=0.07s, 9412 KB]
Best heuristic value: 80 [g=21, 24 evaluated, 23 expanded, t=0.07s, 9412 KB]
Best heuristic value: 79 [g=22, 25 evaluated, 24 expanded, t=0.07s, 9412 KB]
Best heuristic value: 78 [g=23, 26 evaluated, 25 expanded, t=0.07s, 9412 KB]
Best heuristic value: 77 [g=24, 27 evaluated, 26 expanded, t=0.07s, 9412 KB]
Best heuristic value: 76 [g=25, 28 evaluated, 27 expanded, t=0.07s, 9412 KB]
Best heuristic value: 75 [g=26, 29 evaluated, 28 expanded, t=0.07s, 9412 KB]
Best heuristic value: 74 [g=27, 30 evaluated, 29 expanded, t=0.08s, 9412 KB]
Best heuristic value: 73 [g=29, 33 evaluated, 32 expanded, t=0.08s, 9412 KB]
Best heuristic value: 72 [g=30, 34 evaluated, 33 expanded, t=0.08s, 9412 KB]
Best heuristic value: 71 [g=31, 35 evaluated, 34 expanded, t=0.08s, 9412 KB]
Best heuristic value: 70 [g=32, 36 evaluated, 35 expanded, t=0.08s, 9412 KB]
Best heuristic value: 69 [g=39, 43 evaluated, 42 expanded, t=0.08s, 9412 KB]
Best heuristic value: 68 [g=43, 47 evaluated, 46 expanded, t=0.08s, 9412 KB]
Best heuristic value: 67 [g=44, 48 evaluated, 47 expanded, t=0.08s, 9412 KB]
Best heuristic value: 66 [g=45, 49 evaluated, 48 expanded, t=0.08s, 9412 KB]
Best heuristic value: 65 [g=46, 50 evaluated, 49 expanded, t=0.08s, 9412 KB]
Best heuristic value: 64 [g=47, 51 evaluated, 50 expanded, t=0.08s, 9412 KB]
Best heuristic value: 63 [g=51, 55 evaluated, 54 expanded, t=0.08s, 9548 KB]
Best heuristic value: 62 [g=52, 56 evaluated, 55 expanded, t=0.08s, 9548 KB]
Best heuristic value: 61 [g=53, 57 evaluated, 56 expanded, t=0.08s, 9548 KB]
Best heuristic value: 60 [g=54, 58 evaluated, 57 expanded, t=0.08s, 9548 KB]
Best heuristic value: 59 [g=55, 59 evaluated, 58 expanded, t=0.08s, 9548 KB]
Best heuristic value: 53 [g=61, 68 evaluated, 67 expanded, t=0.08s, 9548 KB]
Best heuristic value: 52 [g=71, 83 evaluated, 82 expanded, t=0.08s, 9548 KB]
Best heuristic value: 51 [g=72, 84 evaluated, 83 expanded, t=0.08s, 9548 KB]
Best heuristic value: 50 [g=73, 85 evaluated, 84 expanded, t=0.08s, 9548 KB]
Best heuristic value: 49 [g=74, 86 evaluated, 85 expanded, t=0.08s, 9548 KB]
Best heuristic value: 48 [g=76, 91 evaluated, 90 expanded, t=0.08s, 9548 KB]
Best heuristic value: 47 [g=80, 95 evaluated, 94 expanded, t=0.08s, 9548 KB]
Best heuristic value: 42 [g=81, 96 evaluated, 95 expanded, t=0.08s, 9548 KB]
Best heuristic value: 41 [g=82, 97 evaluated, 96 expanded, t=0.08s, 9548 KB]
Best heuristic value: 40 [g=89, 104 evaluated, 103 expanded, t=0.09s, 9548 KB]
Best heuristic value: 39 [g=90, 105 evaluated, 104 expanded, t=0.09s, 9548 KB]
Best heuristic value: 38 [g=91, 106 evaluated, 105 expanded, t=0.09s, 9548 KB]
Best heuristic value: 37 [g=92, 107 evaluated, 106 expanded, t=0.09s, 9548 KB]
Best heuristic value: 36 [g=93, 108 evaluated, 107 expanded, t=0.09s, 9548 KB]
Best heuristic value: 35 [g=94, 109 evaluated, 108 expanded, t=0.09s, 9548 KB]
Best heuristic value: 34 [g=95, 110 evaluated, 109 expanded, t=0.09s, 9548 KB]
Best heuristic value: 33 [g=101, 119 evaluated, 118 expanded, t=0.09s, 9548 KB]
Best heuristic value: 32 [g=102, 120 evaluated, 119 expanded, t=0.09s, 9548 KB]
Best heuristic value: 31 [g=103, 121 evaluated, 120 expanded, t=0.09s, 9548 KB]
Best heuristic value: 30 [g=104, 122 evaluated, 121 expanded, t=0.09s, 9548 KB]
Best heuristic value: 29 [g=105, 123 evaluated, 122 expanded, t=0.09s, 9548 KB]
Best heuristic value: 28 [g=108, 128 evaluated, 127 expanded, t=0.09s, 9548 KB]
Best heuristic value: 27 [g=109, 129 evaluated, 128 expanded, t=0.09s, 9548 KB]
Best heuristic value: 26 [g=110, 130 evaluated, 129 expanded, t=0.09s, 9548 KB]
Best heuristic value: 25 [g=111, 131 evaluated, 130 expanded, t=0.09s, 9548 KB]
Best heuristic value: 24 [g=112, 132 evaluated, 131 expanded, t=0.09s, 9548 KB]
Best heuristic value: 23 [g=113, 133 evaluated, 132 expanded, t=0.09s, 9548 KB]
Best heuristic value: 22 [g=114, 134 evaluated, 133 expanded, t=0.09s, 9548 KB]
Best heuristic value: 21 [g=115, 135 evaluated, 134 expanded, t=0.09s, 9548 KB]
Best heuristic value: 20 [g=116, 136 evaluated, 135 expanded, t=0.09s, 9548 KB]
Best heuristic value: 19 [g=117, 137 evaluated, 136 expanded, t=0.09s, 9548 KB]
Best heuristic value: 18 [g=136, 156 evaluated, 155 expanded, t=0.1s, 9548 KB]
Best heuristic value: 17 [g=137, 157 evaluated, 156 expanded, t=0.1s, 9548 KB]
Best heuristic value: 16 [g=143, 169 evaluated, 168 expanded, t=0.1s, 9548 KB]
Best heuristic value: 15 [g=144, 170 evaluated, 169 expanded, t=0.1s, 9548 KB]
Best heuristic value: 14 [g=145, 171 evaluated, 170 expanded, t=0.1s, 9548 KB]
Best heuristic value: 13 [g=146, 172 evaluated, 171 expanded, t=0.1s, 9548 KB]
Best heuristic value: 12 [g=147, 173 evaluated, 172 expanded, t=0.1s, 9680 KB]
Best heuristic value: 11 [g=148, 174 evaluated, 173 expanded, t=0.1s, 9680 KB]
Best heuristic value: 10 [g=149, 175 evaluated, 174 expanded, t=0.1s, 9680 KB]
Best heuristic value: 9 [g=150, 176 evaluated, 175 expanded, t=0.1s, 9680 KB]
Best heuristic value: 8 [g=151, 177 evaluated, 176 expanded, t=0.1s, 9680 KB]
Best heuristic value: 7 [g=152, 178 evaluated, 177 expanded, t=0.1s, 9680 KB]
Best heuristic value: 6 [g=153, 179 evaluated, 178 expanded, t=0.1s, 9680 KB]
Best heuristic value: 5 [g=154, 180 evaluated, 179 expanded, t=0.1s, 9680 KB]
Best heuristic value: 4 [g=155, 181 evaluated, 180 expanded, t=0.1s, 9680 KB]
Best heuristic value: 3 [g=156, 182 evaluated, 181 expanded, t=0.1s, 9680 KB]
Best heuristic value: 2 [g=157, 183 evaluated, 182 expanded, t=0.1s, 9680 KB]
Best heuristic value: 1 [g=158, 184 evaluated, 183 expanded, t=0.1s, 9680 KB]
Solution found!
Actual search time: 0.03s [t=0.1s]
anon-take_picture-usv6 area21 disaster1 (1)
anon-communicate_data-usv6 cdm3 disaster1 area21 area21 (1)
anon-navigate_uav-uav6 area21 area34 (1)
anon-take_picture-uav6 area34 disaster9 (1)
anon-navigate_uav-uav6 area34 area9 (1)
anon-communicate_data-uav6 cdm2 disaster9 area9 area13 (1)
anon-navigate_uav-uav6 area9 area29 (1)
anon-take_picture-uav6 area29 disaster8 (1)
anon-communicate_data-uav6 cdm4 disaster8 area29 area33 (1)
anon-navigate_uav-uav6 area29 area24 (1)
anon-take_picture-uav6 area24 disaster7 (1)
anon-navigate_uav-uav6 area24 area9 (1)
anon-communicate_data-uav6 cdm2 disaster7 area9 area13 (1)
anon-navigate_uav-uav6 area9 area20 (1)
anon-take_picture-uav6 area20 disaster6 (1)
anon-navigate_uav-uav6 area20 area9 (1)
anon-communicate_data-uav6 cdm2 disaster6 area9 area13 (1)
anon-navigate_uav-uav6 area9 area5 (1)
anon-take_picture-uav6 area5 disaster5 (1)
anon-navigate_uav-uav6 area5 area9 (1)
anon-communicate_data-uav6 cdm2 disaster5 area9 area13 (1)
anon-navigate_uav-uav6 area9 area2 (1)
anon-take_picture-uav6 area2 disaster4 (1)
anon-communicate_data-uav6 cdm1 disaster4 area2 area1 (1)
anon-navigate_uav-uav6 area2 area23 (1)
anon-take_picture-uav6 area23 disaster3 (1)
anon-communicate_data-uav6 cdm3 disaster3 area23 area21 (1)
anon-navigate_uav-uav6 area23 area32 (1)
anon-take_picture-uav6 area32 disaster2 (1)
anon-navigate_uav-uav6 area32 area9 (1)
anon-communicate_data-uav6 cdm2 disaster2 area9 area13 (1)
anon-navigate_usv-usv2 area1 area2 (1)
anon-navigate_usv-usv2 area2 area4 (1)
anon-navigate_usv-usv2 area4 area9 (1)
anon-sample_water-usv2 usv2store area9 (1)
anon-navigate_usv-usv2 area9 area4 (1)
anon-navigate_usv-usv2 area4 area2 (1)
anon-navigate_usv-usv2 area2 area1 (1)
anon-drop_sample-usv2 usv2store area1 area9 cdm1 (1)
anon-navigate_usv-usv6 area21 area22 (1)
anon-navigate_usv-usv6 area22 area26 (1)
anon-navigate_usv-usv6 area26 area32 (1)
anon-sample_water-usv6 usv6store area32 (1)
anon-navigate_usv-usv6 area32 area26 (1)
anon-navigate_usv-usv6 area26 area22 (1)
anon-navigate_usv-usv6 area22 area21 (1)
anon-drop_sample-usv6 usv6store area21 area32 cdm3 (1)
anon-navigate_usv-usv7 area33 area29 (1)
anon-navigate_usv-usv7 area29 area24 (1)
anon-navigate_usv-usv7 area24 area25 (1)
anon-sample_water-usv7 usv7store area25 (1)
anon-navigate_usv-usv7 area25 area24 (1)
anon-navigate_usv-usv7 area24 area29 (1)
anon-navigate_usv-usv7 area29 area33 (1)
anon-drop_sample-usv7 usv7store area33 area25 cdm4 (1)
anon-pickup_box-ugv1 ugv1store cdm1 area1 box1 (1)
anon-navigate_ugv-ugv1 area1 area3 (1)
anon-navigate_ugv-ugv1 area3 area4 (1)
anon-navigate_ugv-ugv1 area4 area10 (1)
anon-drop_box-ugv1 ugv1store area10 box1 (1)
anon-pickup_box-ugv7 ugv7store cdm4 area33 box6 (1)
anon-navigate_ugv-ugv7 area33 area30 (1)
anon-navigate_ugv-ugv7 area30 area27 (1)
anon-navigate_ugv-ugv7 area27 area8 (1)
anon-navigate_ugv-ugv7 area8 area7 (1)
anon-drop_box-ugv7 ugv7store area7 box6 (1)
anon-navigate_ugv-ugv1 area10 area4 (1)
anon-navigate_ugv-ugv1 area4 area3 (1)
anon-navigate_ugv-ugv1 area3 area1 (1)
anon-pickup_box-ugv1 ugv1store cdm1 area1 box3 (1)
anon-navigate_ugv-ugv1 area1 area3 (1)
anon-navigate_ugv-ugv1 area3 area4 (1)
anon-navigate_ugv-ugv1 area4 area6 (1)
anon-drop_box-ugv1 ugv1store area6 box3 (1)
anon-pickup_box-ugv5 ugv5store cdm3 area21 box5 (1)
anon-navigate_ugv-ugv5 area21 area23 (1)
anon-navigate_ugv-ugv5 area23 area24 (1)
anon-navigate_ugv-ugv5 area24 area30 (1)
anon-navigate_ugv-ugv5 area30 area27 (1)
anon-navigate_ugv-ugv5 area27 area8 (1)
anon-drop_box-ugv5 ugv5store area8 box5 (1)
anon-navigate_usv-usv4 area13 area9 (1)
anon-navigate_usv-usv4 area9 area4 (1)
anon-navigate_usv-usv4 area4 area2 (1)
anon-navigate_usv-usv4 area2 area6 (1)
anon-navigate_usv-usv4 area6 area12 (1)
anon-navigate_usv-usv4 area12 area10 (1)
anon-sample_water-usv4 usv4store area10 (1)
anon-navigate_usv-usv4 area10 area12 (1)
anon-navigate_usv-usv4 area12 area6 (1)
anon-navigate_usv-usv4 area6 area2 (1)
anon-navigate_usv-usv4 area2 area4 (1)
anon-navigate_usv-usv4 area4 area9 (1)
anon-navigate_usv-usv4 area9 area13 (1)
anon-drop_sample-usv4 usv4store area13 area10 cdm2 (1)
anon-navigate_ugv-ugv5 area8 area27 (1)
anon-navigate_ugv-ugv5 area27 area30 (1)
anon-navigate_ugv-ugv5 area30 area33 (1)
anon-pickup_box-ugv5 ugv5store cdm4 area33 box4 (1)
anon-navigate_ugv-ugv5 area33 area30 (1)
anon-navigate_ugv-ugv5 area30 area24 (1)
anon-navigate_ugv-ugv5 area24 area23 (1)
anon-navigate_ugv-ugv5 area23 area25 (1)
anon-navigate_ugv-ugv5 area25 area31 (1)
anon-drop_box-ugv5 ugv5store area31 box4 (1)
anon-navigate_ugv-ugv7 area7 area10 (1)
anon-navigate_ugv-ugv7 area10 area13 (1)
anon-pickup_box-ugv7 ugv7store cdm2 area13 box2 (1)
anon-navigate_ugv-ugv7 area13 area10 (1)
anon-navigate_ugv-ugv7 area10 area7 (1)
anon-navigate_ugv-ugv7 area7 area8 (1)
anon-navigate_ugv-ugv7 area8 area27 (1)
anon-navigate_ugv-ugv7 area27 area30 (1)
anon-navigate_ugv-ugv7 area30 area24 (1)
anon-navigate_ugv-ugv7 area24 area23 (1)
anon-navigate_ugv-ugv7 area23 area35 (1)
anon-drop_box-ugv7 ugv7store area35 box2 (1)
anon-navigate_usv-usv2 area1 area2 (1)
anon-navigate_usv-usv2 area2 area4 (1)
anon-navigate_usv-usv2 area4 area9 (1)
anon-navigate_usv-usv2 area9 area8 (1)
anon-navigate_usv-usv2 area8 area7 (1)
anon-navigate_usv-usv2 area7 area28 (1)
anon-navigate_usv-usv2 area28 area29 (1)
anon-navigate_usv-usv2 area29 area24 (1)
anon-navigate_usv-usv2 area24 area22 (1)
anon-sample_water-usv2 usv2store area22 (1)
anon-navigate_usv-usv2 area22 area24 (1)
anon-navigate_usv-usv2 area24 area29 (1)
anon-navigate_usv-usv2 area29 area28 (1)
anon-navigate_usv-usv2 area28 area7 (1)
anon-navigate_usv-usv2 area7 area8 (1)
anon-navigate_usv-usv2 area8 area9 (1)
anon-navigate_usv-usv2 area9 area4 (1)
anon-navigate_usv-usv2 area4 area2 (1)
anon-navigate_usv-usv2 area2 area1 (1)
anon-drop_sample-usv2 usv2store area1 area22 cdm1 (1)
anon-navigate_usv-usv2 area1 area16 (1)
anon-navigate_usv-usv2 area16 area18 (1)
anon-navigate_usv-usv2 area18 area20 (1)
anon-navigate_usv-usv2 area20 area19 (1)
anon-navigate_usv-usv2 area19 area17 (1)
anon-sample_water-usv2 usv2store area17 (1)
anon-navigate_usv-usv2 area17 area19 (1)
anon-navigate_usv-usv2 area19 area20 (1)
anon-navigate_usv-usv2 area20 area18 (1)
anon-navigate_usv-usv2 area18 area16 (1)
anon-navigate_usv-usv2 area16 area14 (1)
anon-navigate_usv-usv2 area14 area2 (1)
anon-navigate_usv-usv2 area2 area4 (1)
anon-navigate_usv-usv2 area4 area9 (1)
anon-navigate_usv-usv2 area9 area8 (1)
anon-navigate_usv-usv2 area8 area7 (1)
anon-navigate_usv-usv2 area7 area28 (1)
anon-navigate_usv-usv2 area28 area29 (1)
anon-navigate_usv-usv2 area29 area24 (1)
anon-navigate_usv-usv2 area24 area22 (1)
anon-navigate_usv-usv2 area22 area21 (1)
anon-drop_sample-usv2 usv2store area21 area17 cdm3 (1)
Plan length: 159 step(s).
Plan cost: 159
Initial state h value: 97.
Expanded 184 state(s).
Reopened 0 state(s).
Evaluated 185 state(s).
Evaluations: 370
Generated 9557 state(s).
Dead ends: 0 state(s).
Search time: 0.03s
Total time: 0.1s
Solution found.
Peak memory: 9680 KB
