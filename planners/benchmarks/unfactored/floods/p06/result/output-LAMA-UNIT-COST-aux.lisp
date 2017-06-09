INFO     Running translator.
INFO     translator inputs: ['/home/rovane/planners/benchmarks/unfactored/floods/p06/merged-obfuscated-domain.pddl', '/home/rovane/planners/benchmarks/unfactored/floods/p06/merged-obfuscated-problem.pddl']
INFO     translator arguments: []
Parsing...
Parsing: [0.010s CPU, 0.009s wall-clock]
Normalizing task... [0.000s CPU, 0.001s wall-clock]
Instantiating...
Generating Datalog program... [0.000s CPU, 0.003s wall-clock]
Normalizing Datalog program...
Normalizing Datalog program: [0.010s CPU, 0.011s wall-clock]
Preparing model... [0.010s CPU, 0.008s wall-clock]
Generated 251 rules.
Computing model... [0.140s CPU, 0.139s wall-clock]
7629 relevant atoms
2811 auxiliary atoms
10440 final queue length
17292 total queue pushes
Completing instantiation... [0.260s CPU, 0.254s wall-clock]
Instantiating: [0.420s CPU, 0.417s wall-clock]
Computing fact groups...
Finding invariants...
65 initial candidates
Finding invariants: [0.040s CPU, 0.039s wall-clock]
Checking invariant weight... [0.000s CPU, 0.001s wall-clock]
Instantiating groups... [0.010s CPU, 0.009s wall-clock]
Collecting mutex groups... [0.000s CPU, 0.001s wall-clock]
Choosing groups...
365 uncovered facts
Choosing groups: [0.000s CPU, 0.002s wall-clock]
Building translation key... [0.000s CPU, 0.002s wall-clock]
Computing fact groups: [0.060s CPU, 0.061s wall-clock]
Building STRIPS to SAS dictionary... [0.000s CPU, 0.001s wall-clock]
Building dictionary for full mutex groups... [0.000s CPU, 0.001s wall-clock]
Building mutex information...
Building mutex information: [0.000s CPU, 0.001s wall-clock]
Translating task...
Processing axioms...
Simplifying axioms... [0.000s CPU, 0.000s wall-clock]
Processing axioms: [0.020s CPU, 0.011s wall-clock]
Translating task: [0.240s CPU, 0.235s wall-clock]
2848 effect conditions simplified
0 implied preconditions added
Detecting unreachable propositions...
0 operators removed
34 propositions removed
Detecting unreachable propositions: [0.040s CPU, 0.044s wall-clock]
Translator variables: 372
Translator derived variables: 0
Translator facts: 1331
Translator goal facts: 24
Translator mutex groups: 44
Translator total mutex groups size: 675
Translator operators: 6148
Translator axioms: 0
Translator task size: 27714
Translator peak memory: 55268 KB
Writing output... [0.050s CPU, 0.052s wall-clock]
Done! [0.840s CPU, 0.836s wall-clock]
INFO     Running preprocessor.
INFO     preprocessor input: output.sas
INFO     preprocessor arguments: []
Building causal graph...
The causal graph is not acyclic.
228 variables of 372 necessary
0 of 44 mutex groups necessary.
6148 of 6148 operators necessary.
0 of 0 axiom rules necessary.
Building domain transition graphs...
solveable in poly time 0
Building successor generator...
Preprocessor facts: 1043
Preprocessor derived variables: 0
Preprocessor task size: 24620
Writing output...
done
INFO     Running search.
INFO     search input: output
INFO     search executable: /home/rovane/planners/cmap/planning/fd/src/search/downward-release
INFO     search arguments: ['--heuristic', 'hlm,hff=lm_ff_syn(lm_rhw(reasonable_orders=true,lm_cost_type=1,cost_type=1))', '--search', 'lazy_greedy(hff,hlm,preferred=[hff,hlm])', '--internal-plan-file', '/home/rovane/planners/benchmarks/unfactored/floods/p06/result/plan-LAMA-UNIT-COST.lisp']
reading input... [t=0s]
Simplifying transitions... done!
done reading input! [t=0.08s]
building causal graph...done! [t=0.08s]
packing state variables...done! [t=0.08s]
Variables: 228
Facts: 1043
Bytes per state: 40
done initalizing global data [t=0.08s]
Initializing Exploration...
Generating landmarks using the RPG/SAS+ approach
approx. reasonable orders
approx. obedient reasonable orders
Removed 0 reasonable or obedient reasonable orders
Landmarks generation time: 0.0311633s
Discovered 48 landmarks, of which 0 are disjunctive and 0 are conjunctive 
24 edges
Initializing LAMA-FF Synergy Object
Initializing landmarks count heuristic...
24 initial landmarks, 24 goal landmarks
Conducting lazy best first search, (real) bound = 2147483647
Best heuristic value: 111 [g=0, 1 evaluated, 0 expanded, t=0.12s, 13788 KB]
Best heuristic value: 110 [g=1, 2 evaluated, 1 expanded, t=0.12s, 13788 KB]
Best heuristic value: 109 [g=2, 3 evaluated, 2 expanded, t=0.12s, 13788 KB]
Best heuristic value: 108 [g=3, 4 evaluated, 3 expanded, t=0.12s, 13788 KB]
Best heuristic value: 107 [g=4, 5 evaluated, 4 expanded, t=0.12s, 13788 KB]
Best heuristic value: 106 [g=5, 6 evaluated, 5 expanded, t=0.12s, 13788 KB]
Best heuristic value: 105 [g=7, 8 evaluated, 7 expanded, t=0.12s, 13788 KB]
Best heuristic value: 104 [g=8, 9 evaluated, 8 expanded, t=0.12s, 13788 KB]
Best heuristic value: 103 [g=9, 10 evaluated, 9 expanded, t=0.12s, 13788 KB]
Best heuristic value: 102 [g=11, 12 evaluated, 11 expanded, t=0.12s, 13788 KB]
Best heuristic value: 101 [g=12, 13 evaluated, 12 expanded, t=0.12s, 13788 KB]
Best heuristic value: 100 [g=13, 14 evaluated, 13 expanded, t=0.12s, 13788 KB]
Best heuristic value: 99 [g=15, 16 evaluated, 15 expanded, t=0.12s, 13788 KB]
Best heuristic value: 98 [g=16, 17 evaluated, 16 expanded, t=0.12s, 13788 KB]
Best heuristic value: 97 [g=17, 18 evaluated, 17 expanded, t=0.12s, 13788 KB]
Best heuristic value: 96 [g=18, 19 evaluated, 18 expanded, t=0.12s, 13788 KB]
Best heuristic value: 95 [g=19, 20 evaluated, 19 expanded, t=0.12s, 13788 KB]
Best heuristic value: 94 [g=20, 21 evaluated, 20 expanded, t=0.12s, 13788 KB]
Best heuristic value: 93 [g=22, 24 evaluated, 23 expanded, t=0.12s, 13788 KB]
Best heuristic value: 92 [g=23, 25 evaluated, 24 expanded, t=0.12s, 13788 KB]
Best heuristic value: 91 [g=24, 26 evaluated, 25 expanded, t=0.12s, 13788 KB]
Best heuristic value: 90 [g=25, 27 evaluated, 26 expanded, t=0.12s, 13788 KB]
Best heuristic value: 89 [g=26, 28 evaluated, 27 expanded, t=0.12s, 13788 KB]
Best heuristic value: 88 [g=27, 29 evaluated, 28 expanded, t=0.12s, 13788 KB]
Best heuristic value: 87 [g=29, 32 evaluated, 31 expanded, t=0.12s, 13788 KB]
Best heuristic value: 86 [g=30, 33 evaluated, 32 expanded, t=0.12s, 13788 KB]
Best heuristic value: 85 [g=31, 34 evaluated, 33 expanded, t=0.12s, 13788 KB]
Best heuristic value: 84 [g=33, 36 evaluated, 35 expanded, t=0.12s, 13788 KB]
Best heuristic value: 83 [g=34, 37 evaluated, 36 expanded, t=0.12s, 13788 KB]
Best heuristic value: 82 [g=35, 38 evaluated, 37 expanded, t=0.12s, 13788 KB]
Best heuristic value: 77 [g=38, 43 evaluated, 42 expanded, t=0.12s, 13788 KB]
Best heuristic value: 76 [g=50, 57 evaluated, 56 expanded, t=0.13s, 13788 KB]
Best heuristic value: 75 [g=51, 58 evaluated, 57 expanded, t=0.13s, 13788 KB]
Best heuristic value: 74 [g=52, 59 evaluated, 58 expanded, t=0.13s, 13788 KB]
Best heuristic value: 66 [g=55, 62 evaluated, 61 expanded, t=0.13s, 13788 KB]
Best heuristic value: 65 [g=56, 63 evaluated, 62 expanded, t=0.13s, 13788 KB]
Best heuristic value: 62 [g=60, 67 evaluated, 66 expanded, t=0.13s, 13788 KB]
Best heuristic value: 60 [g=70, 77 evaluated, 76 expanded, t=0.13s, 13788 KB]
Best heuristic value: 59 [g=75, 82 evaluated, 81 expanded, t=0.13s, 13788 KB]
Best heuristic value: 58 [g=79, 90 evaluated, 89 expanded, t=0.14s, 13788 KB]
Best heuristic value: 57 [g=82, 93 evaluated, 92 expanded, t=0.14s, 13800 KB]
Best heuristic value: 56 [g=83, 94 evaluated, 93 expanded, t=0.14s, 13800 KB]
Best heuristic value: 55 [g=84, 96 evaluated, 95 expanded, t=0.14s, 13800 KB]
Best heuristic value: 54 [g=85, 97 evaluated, 96 expanded, t=0.14s, 13800 KB]
Best heuristic value: 53 [g=86, 98 evaluated, 97 expanded, t=0.14s, 13800 KB]
Best heuristic value: 52 [g=87, 99 evaluated, 98 expanded, t=0.14s, 13800 KB]
Best heuristic value: 51 [g=90, 102 evaluated, 101 expanded, t=0.14s, 13800 KB]
Best heuristic value: 50 [g=91, 103 evaluated, 102 expanded, t=0.14s, 13800 KB]
Best heuristic value: 48 [g=92, 104 evaluated, 103 expanded, t=0.14s, 13800 KB]
Best heuristic value: 47 [g=94, 110 evaluated, 109 expanded, t=0.14s, 13800 KB]
Best heuristic value: 46 [g=95, 111 evaluated, 110 expanded, t=0.14s, 13800 KB]
Best heuristic value: 45 [g=96, 112 evaluated, 111 expanded, t=0.14s, 13800 KB]
Best heuristic value: 44 [g=97, 113 evaluated, 112 expanded, t=0.14s, 13800 KB]
Best heuristic value: 38 [g=98, 114 evaluated, 113 expanded, t=0.14s, 13800 KB]
Best heuristic value: 37 [g=111, 127 evaluated, 126 expanded, t=0.14s, 13800 KB]
Best heuristic value: 36 [g=112, 128 evaluated, 127 expanded, t=0.14s, 13800 KB]
Best heuristic value: 35 [g=113, 129 evaluated, 128 expanded, t=0.14s, 13800 KB]
Best heuristic value: 34 [g=116, 135 evaluated, 134 expanded, t=0.14s, 13800 KB]
Best heuristic value: 33 [g=117, 136 evaluated, 135 expanded, t=0.14s, 13800 KB]
Best heuristic value: 32 [g=118, 137 evaluated, 136 expanded, t=0.14s, 13800 KB]
Best heuristic value: 31 [g=119, 138 evaluated, 137 expanded, t=0.14s, 13800 KB]
Best heuristic value: 30 [g=120, 139 evaluated, 138 expanded, t=0.14s, 13800 KB]
Best heuristic value: 29 [g=121, 140 evaluated, 139 expanded, t=0.14s, 13800 KB]
Best heuristic value: 28 [g=122, 141 evaluated, 140 expanded, t=0.14s, 13800 KB]
Best heuristic value: 27 [g=123, 142 evaluated, 141 expanded, t=0.14s, 13800 KB]
Best heuristic value: 26 [g=124, 143 evaluated, 142 expanded, t=0.14s, 13800 KB]
Best heuristic value: 25 [g=131, 153 evaluated, 152 expanded, t=0.14s, 13800 KB]
Best heuristic value: 24 [g=132, 154 evaluated, 153 expanded, t=0.14s, 13800 KB]
Best heuristic value: 23 [g=133, 155 evaluated, 154 expanded, t=0.14s, 13800 KB]
Best heuristic value: 22 [g=134, 156 evaluated, 155 expanded, t=0.14s, 13932 KB]
Best heuristic value: 21 [g=135, 157 evaluated, 156 expanded, t=0.14s, 13932 KB]
Best heuristic value: 20 [g=136, 158 evaluated, 157 expanded, t=0.14s, 13932 KB]
Best heuristic value: 19 [g=137, 159 evaluated, 158 expanded, t=0.14s, 13932 KB]
Best heuristic value: 18 [g=138, 160 evaluated, 159 expanded, t=0.14s, 13932 KB]
Best heuristic value: 17 [g=139, 161 evaluated, 160 expanded, t=0.14s, 13932 KB]
Best heuristic value: 16 [g=140, 162 evaluated, 161 expanded, t=0.14s, 13932 KB]
Best heuristic value: 15 [g=141, 163 evaluated, 162 expanded, t=0.15s, 13932 KB]
Best heuristic value: 14 [g=154, 193 evaluated, 192 expanded, t=0.15s, 13932 KB]
Best heuristic value: 13 [g=155, 194 evaluated, 193 expanded, t=0.15s, 13932 KB]
Best heuristic value: 12 [g=167, 209 evaluated, 208 expanded, t=0.16s, 13932 KB]
Best heuristic value: 11 [g=168, 210 evaluated, 209 expanded, t=0.16s, 13932 KB]
Best heuristic value: 10 [g=169, 211 evaluated, 210 expanded, t=0.16s, 13932 KB]
Best heuristic value: 9 [g=170, 212 evaluated, 211 expanded, t=0.16s, 13932 KB]
Best heuristic value: 8 [g=171, 213 evaluated, 212 expanded, t=0.16s, 13932 KB]
Best heuristic value: 7 [g=172, 214 evaluated, 213 expanded, t=0.16s, 13932 KB]
Best heuristic value: 6 [g=173, 215 evaluated, 214 expanded, t=0.16s, 13932 KB]
Best heuristic value: 5 [g=174, 216 evaluated, 215 expanded, t=0.16s, 13932 KB]
Best heuristic value: 4 [g=175, 217 evaluated, 216 expanded, t=0.16s, 13932 KB]
Best heuristic value: 3 [g=176, 218 evaluated, 217 expanded, t=0.16s, 13932 KB]
Best heuristic value: 2 [g=177, 219 evaluated, 218 expanded, t=0.16s, 13932 KB]
Best heuristic value: 1 [g=178, 220 evaluated, 219 expanded, t=0.16s, 13932 KB]
Solution found!
Actual search time: 0.04s [t=0.16s]
anon-take_picture-usv5 area21 disaster10 (1)
anon-communicate_data-usv5 cdm3 disaster10 area21 area21 (1)
anon-navigate_uav-uav5 area21 area9 (1)
anon-take_picture-uav5 area9 disaster9 (1)
anon-communicate_data-uav5 cdm2 disaster9 area9 area13 (1)
anon-navigate_uav-uav5 area9 area7 (1)
anon-take_picture-uav5 area7 disaster8 (1)
anon-navigate_uav-uav5 area7 area9 (1)
anon-communicate_data-uav5 cdm2 disaster8 area9 area13 (1)
anon-navigate_uav-uav5 area9 area40 (1)
anon-take_picture-uav5 area40 disaster7 (1)
anon-navigate_uav-uav5 area40 area9 (1)
anon-communicate_data-uav5 cdm2 disaster7 area9 area13 (1)
anon-navigate_uav-uav5 area9 area17 (1)
anon-take_picture-uav5 area17 disaster6 (1)
anon-navigate_uav-uav5 area17 area9 (1)
anon-communicate_data-uav5 cdm2 disaster6 area9 area13 (1)
anon-navigate_uav-uav5 area9 area3 (1)
anon-take_picture-uav5 area3 disaster5 (1)
anon-communicate_data-uav5 cdm1 disaster5 area3 area1 (1)
anon-navigate_uav-uav5 area3 area28 (1)
anon-take_picture-uav5 area28 disaster4 (1)
anon-navigate_uav-uav5 area28 area9 (1)
anon-communicate_data-uav5 cdm2 disaster4 area9 area13 (1)
anon-navigate_uav-uav5 area9 area10 (1)
anon-take_picture-uav5 area10 disaster3 (1)
anon-communicate_data-uav5 cdm2 disaster3 area10 area13 (1)
anon-navigate_uav-uav5 area10 area32 (1)
anon-take_picture-uav5 area32 disaster2 (1)
anon-navigate_uav-uav5 area32 area9 (1)
anon-communicate_data-uav5 cdm2 disaster2 area9 area13 (1)
anon-navigate_uav-uav5 area9 area31 (1)
anon-take_picture-uav5 area31 disaster1 (1)
anon-navigate_uav-uav5 area31 area9 (1)
anon-communicate_data-uav5 cdm2 disaster1 area9 area13 (1)
anon-pickup_box-ugv7 ugv7store cdm4 area33 box4 (1)
anon-navigate_ugv-ugv7 area33 area30 (1)
anon-navigate_ugv-ugv7 area30 area24 (1)
anon-drop_box-ugv7 ugv7store area24 box4 (1)
anon-pickup_box-ugv5 ugv5store cdm3 area21 box7 (1)
anon-navigate_ugv-ugv5 area21 area36 (1)
anon-navigate_ugv-ugv5 area36 area35 (1)
anon-drop_box-ugv5 ugv5store area35 box7 (1)
anon-navigate_usv-usv3 area13 area9 (1)
anon-navigate_usv-usv3 area9 area8 (1)
anon-navigate_usv-usv3 area8 area7 (1)
anon-sample_water-usv3 usv3store area7 (1)
anon-navigate_usv-usv3 area7 area8 (1)
anon-navigate_usv-usv3 area8 area9 (1)
anon-navigate_usv-usv3 area9 area13 (1)
anon-drop_sample-usv3 usv3store area13 area7 cdm2 (1)
anon-navigate_usv-usv5 area21 area36 (1)
anon-navigate_usv-usv5 area36 area38 (1)
anon-navigate_usv-usv5 area38 area40 (1)
anon-sample_water-usv5 usv5store area40 (1)
anon-navigate_usv-usv5 area40 area38 (1)
anon-navigate_usv-usv5 area38 area36 (1)
anon-navigate_usv-usv5 area36 area21 (1)
anon-drop_sample-usv5 usv5store area21 area40 cdm3 (1)
anon-navigate_usv-usv7 area33 area29 (1)
anon-navigate_usv-usv7 area29 area24 (1)
anon-navigate_usv-usv7 area24 area22 (1)
anon-sample_water-usv7 usv7store area22 (1)
anon-navigate_usv-usv7 area22 area24 (1)
anon-navigate_usv-usv7 area24 area29 (1)
anon-navigate_usv-usv7 area29 area33 (1)
anon-drop_sample-usv7 usv7store area33 area22 cdm4 (1)
anon-navigate_usv-usv5 area21 area22 (1)
anon-navigate_usv-usv5 area22 area24 (1)
anon-navigate_usv-usv5 area24 area29 (1)
anon-navigate_usv-usv5 area29 area28 (1)
anon-sample_water-usv5 usv5store area28 (1)
anon-navigate_usv-usv5 area28 area29 (1)
anon-navigate_usv-usv5 area29 area24 (1)
anon-navigate_usv-usv5 area24 area22 (1)
anon-navigate_usv-usv5 area22 area21 (1)
anon-drop_sample-usv5 usv5store area21 area28 cdm3 (1)
anon-pickup_box-ugv3 ugv3store cdm2 area13 box2 (1)
anon-navigate_ugv-ugv3 area13 area10 (1)
anon-navigate_ugv-ugv3 area10 area7 (1)
anon-navigate_ugv-ugv3 area7 area28 (1)
anon-navigate_ugv-ugv3 area28 area27 (1)
anon-drop_box-ugv3 ugv3store area27 box2 (1)
anon-pickup_box-ugv1 ugv1store cdm1 area1 box1 (1)
anon-navigate_ugv-ugv1 area1 area3 (1)
anon-navigate_ugv-ugv1 area3 area15 (1)
anon-navigate_ugv-ugv1 area15 area18 (1)
anon-navigate_ugv-ugv1 area18 area37 (1)
anon-navigate_ugv-ugv1 area37 area36 (1)
anon-navigate_ugv-ugv1 area36 area21 (1)
anon-drop_box-ugv1 ugv1store area21 box1 (1)
anon-pickup_box-ugv1 ugv1store cdm3 area21 box5 (1)
anon-navigate_ugv-ugv1 area21 area36 (1)
anon-navigate_ugv-ugv1 area36 area37 (1)
anon-navigate_ugv-ugv1 area37 area18 (1)
anon-navigate_ugv-ugv1 area18 area20 (1)
anon-drop_box-ugv1 ugv1store area20 box5 (1)
anon-navigate_usv-usv7 area33 area29 (1)
anon-navigate_usv-usv7 area29 area28 (1)
anon-navigate_usv-usv7 area28 area27 (1)
anon-navigate_usv-usv7 area27 area8 (1)
anon-navigate_usv-usv7 area8 area9 (1)
anon-navigate_usv-usv7 area9 area4 (1)
anon-navigate_usv-usv7 area4 area2 (1)
anon-sample_water-usv7 usv7store area2 (1)
anon-navigate_usv-usv7 area2 area4 (1)
anon-navigate_usv-usv7 area4 area9 (1)
anon-navigate_usv-usv7 area9 area8 (1)
anon-navigate_usv-usv7 area8 area27 (1)
anon-navigate_usv-usv7 area27 area28 (1)
anon-navigate_usv-usv7 area28 area29 (1)
anon-navigate_usv-usv7 area29 area33 (1)
anon-drop_sample-usv7 usv7store area33 area2 cdm4 (1)
anon-navigate_ugv-ugv3 area27 area30 (1)
anon-navigate_ugv-ugv3 area30 area33 (1)
anon-pickup_box-ugv3 ugv3store cdm4 area33 box6 (1)
anon-navigate_ugv-ugv3 area33 area30 (1)
anon-navigate_ugv-ugv3 area30 area27 (1)
anon-navigate_ugv-ugv3 area27 area28 (1)
anon-navigate_ugv-ugv3 area28 area7 (1)
anon-navigate_ugv-ugv3 area7 area10 (1)
anon-navigate_ugv-ugv3 area10 area4 (1)
anon-navigate_ugv-ugv3 area4 area6 (1)
anon-drop_box-ugv3 ugv3store area6 box6 (1)
anon-navigate_ugv-ugv5 area35 area36 (1)
anon-navigate_ugv-ugv5 area36 area37 (1)
anon-navigate_ugv-ugv5 area37 area18 (1)
anon-navigate_ugv-ugv5 area18 area15 (1)
anon-navigate_ugv-ugv5 area15 area3 (1)
anon-navigate_ugv-ugv5 area3 area1 (1)
anon-pickup_box-ugv5 ugv5store cdm1 area1 box3 (1)
anon-navigate_ugv-ugv5 area1 area3 (1)
anon-navigate_ugv-ugv5 area3 area15 (1)
anon-navigate_ugv-ugv5 area15 area18 (1)
anon-navigate_ugv-ugv5 area18 area37 (1)
anon-navigate_ugv-ugv5 area37 area36 (1)
anon-navigate_ugv-ugv5 area36 area35 (1)
anon-navigate_ugv-ugv5 area35 area38 (1)
anon-navigate_ugv-ugv5 area38 area40 (1)
anon-navigate_ugv-ugv5 area40 area39 (1)
anon-drop_box-ugv5 ugv5store area39 box3 (1)
anon-navigate_usv-usv5 area21 area22 (1)
anon-navigate_usv-usv5 area22 area26 (1)
anon-sample_water-usv5 usv5store area26 (1)
anon-navigate_usv-usv5 area26 area22 (1)
anon-navigate_usv-usv5 area22 area24 (1)
anon-navigate_usv-usv5 area24 area29 (1)
anon-navigate_usv-usv5 area29 area28 (1)
anon-navigate_usv-usv5 area28 area27 (1)
anon-navigate_usv-usv5 area27 area8 (1)
anon-navigate_usv-usv5 area8 area9 (1)
anon-navigate_usv-usv5 area9 area4 (1)
anon-navigate_usv-usv5 area4 area2 (1)
anon-navigate_usv-usv5 area2 area1 (1)
anon-drop_sample-usv5 usv5store area1 area26 cdm1 (1)
anon-navigate_usv-usv5 area1 area16 (1)
anon-navigate_usv-usv5 area16 area18 (1)
anon-navigate_usv-usv5 area18 area20 (1)
anon-navigate_usv-usv5 area20 area19 (1)
anon-navigate_usv-usv5 area19 area17 (1)
anon-navigate_usv-usv5 area17 area38 (1)
anon-navigate_usv-usv5 area38 area36 (1)
anon-navigate_usv-usv5 area36 area34 (1)
anon-navigate_usv-usv5 area34 area22 (1)
anon-navigate_usv-usv5 area22 area26 (1)
anon-navigate_usv-usv5 area26 area32 (1)
anon-sample_water-usv5 usv5store area32 (1)
anon-navigate_usv-usv5 area32 area26 (1)
anon-navigate_usv-usv5 area26 area22 (1)
anon-navigate_usv-usv5 area22 area24 (1)
anon-navigate_usv-usv5 area24 area29 (1)
anon-navigate_usv-usv5 area29 area28 (1)
anon-navigate_usv-usv5 area28 area27 (1)
anon-navigate_usv-usv5 area27 area8 (1)
anon-navigate_usv-usv5 area8 area9 (1)
anon-navigate_usv-usv5 area9 area4 (1)
anon-navigate_usv-usv5 area4 area2 (1)
anon-navigate_usv-usv5 area2 area1 (1)
anon-drop_sample-usv5 usv5store area1 area32 cdm1 (1)
Plan length: 179 step(s).
Plan cost: 179
Initial state h value: 111.
Expanded 220 state(s).
Reopened 0 state(s).
Evaluated 221 state(s).
Evaluations: 442
Generated 21522 state(s).
Dead ends: 0 state(s).
Search time: 0.04s
Total time: 0.16s
Solution found.
Peak memory: 14068 KB
