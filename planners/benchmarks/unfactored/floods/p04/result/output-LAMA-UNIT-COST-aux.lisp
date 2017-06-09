INFO     Running translator.
INFO     translator inputs: ['/home/rovane/planners/benchmarks/unfactored/floods/p04/merged-obfuscated-domain.pddl', '/home/rovane/planners/benchmarks/unfactored/floods/p04/merged-obfuscated-problem.pddl']
INFO     translator arguments: []
Parsing...
Parsing: [0.010s CPU, 0.007s wall-clock]
Normalizing task... [0.000s CPU, 0.001s wall-clock]
Instantiating...
Generating Datalog program... [0.000s CPU, 0.002s wall-clock]
Normalizing Datalog program...
Normalizing Datalog program: [0.010s CPU, 0.009s wall-clock]
Preparing model... [0.000s CPU, 0.006s wall-clock]
Generated 203 rules.
Computing model... [0.090s CPU, 0.087s wall-clock]
5055 relevant atoms
1768 auxiliary atoms
6823 final queue length
10949 total queue pushes
Completing instantiation... [0.140s CPU, 0.143s wall-clock]
Instantiating: [0.250s CPU, 0.250s wall-clock]
Computing fact groups...
Finding invariants...
57 initial candidates
Finding invariants: [0.010s CPU, 0.017s wall-clock]
Checking invariant weight... [0.000s CPU, 0.000s wall-clock]
Instantiating groups... [0.010s CPU, 0.004s wall-clock]
Collecting mutex groups... [0.000s CPU, 0.000s wall-clock]
Choosing groups...
248 uncovered facts
Choosing groups: [0.000s CPU, 0.001s wall-clock]
Building translation key... [0.010s CPU, 0.002s wall-clock]
Computing fact groups: [0.030s CPU, 0.029s wall-clock]
Building STRIPS to SAS dictionary... [0.000s CPU, 0.001s wall-clock]
Building dictionary for full mutex groups... [0.000s CPU, 0.001s wall-clock]
Building mutex information...
Building mutex information: [0.000s CPU, 0.001s wall-clock]
Translating task...
Processing axioms...
Simplifying axioms... [0.000s CPU, 0.000s wall-clock]
Processing axioms: [0.000s CPU, 0.007s wall-clock]
Translating task: [0.140s CPU, 0.138s wall-clock]
1322 effect conditions simplified
0 implied preconditions added
Detecting unreachable propositions...
0 operators removed
27 propositions removed
Detecting unreachable propositions: [0.020s CPU, 0.024s wall-clock]
Translator variables: 254
Translator derived variables: 0
Translator facts: 857
Translator goal facts: 18
Translator mutex groups: 23
Translator total mutex groups size: 395
Translator operators: 4030
Translator axioms: 0
Translator task size: 16629
Translator peak memory: 45092 KB
Writing output... [0.030s CPU, 0.030s wall-clock]
Done! [0.490s CPU, 0.489s wall-clock]
INFO     Running preprocessor.
INFO     preprocessor input: output.sas
INFO     preprocessor arguments: []
Building causal graph...
The causal graph is not acyclic.
173 variables of 254 necessary
0 of 23 mutex groups necessary.
4030 of 4030 operators necessary.
0 of 0 axiom rules necessary.
Building domain transition graphs...
solveable in poly time 0
Building successor generator...
Preprocessor facts: 695
Preprocessor derived variables: 0
Preprocessor task size: 15159
Writing output...
done
INFO     Running search.
INFO     search input: output
INFO     search executable: /home/rovane/planners/cmap/planning/fd/src/search/downward-release
INFO     search arguments: ['--heuristic', 'hlm,hff=lm_ff_syn(lm_rhw(reasonable_orders=true,lm_cost_type=1,cost_type=1))', '--search', 'lazy_greedy(hff,hlm,preferred=[hff,hlm])', '--internal-plan-file', '/home/rovane/planners/benchmarks/unfactored/floods/p04/result/plan-LAMA-UNIT-COST.lisp']
reading input... [t=0s]
Simplifying transitions... done!
done reading input! [t=0.04s]
building causal graph...done! [t=0.04s]
packing state variables...done! [t=0.04s]
Variables: 173
Facts: 695
Bytes per state: 32
done initalizing global data [t=0.04s]
Initializing Exploration...
Generating landmarks using the RPG/SAS+ approach
approx. reasonable orders
approx. obedient reasonable orders
Removed 0 reasonable or obedient reasonable orders
Landmarks generation time: 0.0144775s
Discovered 36 landmarks, of which 0 are disjunctive and 0 are conjunctive 
18 edges
Initializing LAMA-FF Synergy Object
Initializing landmarks count heuristic...
18 initial landmarks, 18 goal landmarks
Conducting lazy best first search, (real) bound = 2147483647
Best heuristic value: 79 [g=0, 1 evaluated, 0 expanded, t=0.06s, 8328 KB]
Best heuristic value: 78 [g=1, 2 evaluated, 1 expanded, t=0.06s, 8328 KB]
Best heuristic value: 77 [g=2, 3 evaluated, 2 expanded, t=0.06s, 8328 KB]
Best heuristic value: 76 [g=3, 4 evaluated, 3 expanded, t=0.06s, 8328 KB]
Best heuristic value: 75 [g=4, 5 evaluated, 4 expanded, t=0.06s, 8328 KB]
Best heuristic value: 74 [g=5, 6 evaluated, 5 expanded, t=0.06s, 8328 KB]
Best heuristic value: 73 [g=6, 7 evaluated, 6 expanded, t=0.06s, 8332 KB]
Best heuristic value: 72 [g=8, 10 evaluated, 9 expanded, t=0.06s, 8332 KB]
Best heuristic value: 71 [g=9, 11 evaluated, 10 expanded, t=0.06s, 8332 KB]
Best heuristic value: 70 [g=10, 12 evaluated, 11 expanded, t=0.06s, 8332 KB]
Best heuristic value: 69 [g=12, 14 evaluated, 13 expanded, t=0.06s, 8332 KB]
Best heuristic value: 68 [g=13, 15 evaluated, 14 expanded, t=0.06s, 8332 KB]
Best heuristic value: 67 [g=14, 16 evaluated, 15 expanded, t=0.06s, 8332 KB]
Best heuristic value: 66 [g=15, 17 evaluated, 16 expanded, t=0.06s, 8332 KB]
Best heuristic value: 65 [g=16, 18 evaluated, 17 expanded, t=0.06s, 8332 KB]
Best heuristic value: 64 [g=17, 19 evaluated, 18 expanded, t=0.06s, 8332 KB]
Best heuristic value: 63 [g=19, 22 evaluated, 21 expanded, t=0.06s, 8332 KB]
Best heuristic value: 62 [g=20, 23 evaluated, 22 expanded, t=0.06s, 8332 KB]
Best heuristic value: 61 [g=21, 24 evaluated, 23 expanded, t=0.06s, 8332 KB]
Best heuristic value: 60 [g=22, 25 evaluated, 24 expanded, t=0.06s, 8332 KB]
Best heuristic value: 59 [g=23, 26 evaluated, 25 expanded, t=0.06s, 8332 KB]
Best heuristic value: 58 [g=24, 27 evaluated, 26 expanded, t=0.06s, 8332 KB]
Best heuristic value: 57 [g=26, 30 evaluated, 29 expanded, t=0.06s, 8332 KB]
Best heuristic value: 56 [g=27, 31 evaluated, 30 expanded, t=0.06s, 8332 KB]
Best heuristic value: 55 [g=28, 32 evaluated, 31 expanded, t=0.06s, 8332 KB]
Best heuristic value: 53 [g=33, 37 evaluated, 36 expanded, t=0.06s, 8332 KB]
Best heuristic value: 52 [g=34, 38 evaluated, 37 expanded, t=0.06s, 8332 KB]
Best heuristic value: 50 [g=35, 39 evaluated, 38 expanded, t=0.06s, 8332 KB]
Best heuristic value: 49 [g=40, 44 evaluated, 43 expanded, t=0.06s, 8332 KB]
Best heuristic value: 48 [g=41, 45 evaluated, 44 expanded, t=0.06s, 8332 KB]
Best heuristic value: 47 [g=46, 50 evaluated, 49 expanded, t=0.06s, 8332 KB]
Best heuristic value: 46 [g=47, 51 evaluated, 50 expanded, t=0.06s, 8332 KB]
Best heuristic value: 45 [g=48, 52 evaluated, 51 expanded, t=0.06s, 8332 KB]
Best heuristic value: 44 [g=50, 58 evaluated, 57 expanded, t=0.06s, 8332 KB]
Best heuristic value: 43 [g=53, 61 evaluated, 60 expanded, t=0.06s, 8332 KB]
Best heuristic value: 42 [g=54, 62 evaluated, 61 expanded, t=0.06s, 8332 KB]
Best heuristic value: 41 [g=55, 64 evaluated, 63 expanded, t=0.06s, 8332 KB]
Best heuristic value: 40 [g=56, 65 evaluated, 64 expanded, t=0.06s, 8332 KB]
Best heuristic value: 39 [g=57, 66 evaluated, 65 expanded, t=0.06s, 8332 KB]
Best heuristic value: 38 [g=58, 67 evaluated, 66 expanded, t=0.06s, 8332 KB]
Best heuristic value: 37 [g=59, 68 evaluated, 67 expanded, t=0.06s, 8464 KB]
Best heuristic value: 36 [g=60, 69 evaluated, 68 expanded, t=0.06s, 8464 KB]
Best heuristic value: 34 [g=61, 71 evaluated, 70 expanded, t=0.06s, 8464 KB]
Best heuristic value: 33 [g=65, 83 evaluated, 82 expanded, t=0.07s, 8464 KB]
Best heuristic value: 32 [g=66, 84 evaluated, 83 expanded, t=0.07s, 8464 KB]
Best heuristic value: 31 [g=67, 85 evaluated, 84 expanded, t=0.07s, 8464 KB]
Best heuristic value: 30 [g=68, 86 evaluated, 85 expanded, t=0.07s, 8464 KB]
Best heuristic value: 29 [g=69, 87 evaluated, 86 expanded, t=0.07s, 8464 KB]
Best heuristic value: 28 [g=70, 88 evaluated, 87 expanded, t=0.07s, 8464 KB]
Best heuristic value: 27 [g=80, 98 evaluated, 97 expanded, t=0.07s, 8464 KB]
Best heuristic value: 26 [g=81, 99 evaluated, 98 expanded, t=0.07s, 8464 KB]
Best heuristic value: 25 [g=82, 100 evaluated, 99 expanded, t=0.07s, 8464 KB]
Best heuristic value: 24 [g=83, 101 evaluated, 100 expanded, t=0.07s, 8464 KB]
Best heuristic value: 23 [g=84, 102 evaluated, 101 expanded, t=0.07s, 8464 KB]
Best heuristic value: 22 [g=85, 103 evaluated, 102 expanded, t=0.07s, 8464 KB]
Best heuristic value: 21 [g=86, 104 evaluated, 103 expanded, t=0.07s, 8464 KB]
Best heuristic value: 20 [g=87, 105 evaluated, 104 expanded, t=0.07s, 8464 KB]
Best heuristic value: 19 [g=88, 106 evaluated, 105 expanded, t=0.07s, 8464 KB]
Best heuristic value: 18 [g=89, 107 evaluated, 106 expanded, t=0.07s, 8464 KB]
Best heuristic value: 17 [g=90, 108 evaluated, 107 expanded, t=0.07s, 8464 KB]
Best heuristic value: 16 [g=105, 129 evaluated, 128 expanded, t=0.07s, 8464 KB]
Best heuristic value: 15 [g=106, 130 evaluated, 129 expanded, t=0.07s, 8596 KB]
Best heuristic value: 14 [g=107, 131 evaluated, 130 expanded, t=0.08s, 8596 KB]
Best heuristic value: 13 [g=117, 144 evaluated, 143 expanded, t=0.08s, 8596 KB]
Best heuristic value: 12 [g=118, 145 evaluated, 144 expanded, t=0.08s, 8596 KB]
Best heuristic value: 11 [g=119, 146 evaluated, 145 expanded, t=0.08s, 8596 KB]
Best heuristic value: 10 [g=120, 147 evaluated, 146 expanded, t=0.08s, 8596 KB]
Best heuristic value: 9 [g=121, 148 evaluated, 147 expanded, t=0.08s, 8596 KB]
Best heuristic value: 8 [g=122, 149 evaluated, 148 expanded, t=0.08s, 8596 KB]
Best heuristic value: 7 [g=123, 150 evaluated, 149 expanded, t=0.08s, 8596 KB]
Best heuristic value: 6 [g=124, 151 evaluated, 150 expanded, t=0.08s, 8596 KB]
Best heuristic value: 5 [g=125, 152 evaluated, 151 expanded, t=0.08s, 8596 KB]
Best heuristic value: 4 [g=126, 153 evaluated, 152 expanded, t=0.08s, 8596 KB]
Best heuristic value: 3 [g=127, 154 evaluated, 153 expanded, t=0.08s, 8596 KB]
Best heuristic value: 2 [g=128, 155 evaluated, 154 expanded, t=0.08s, 8596 KB]
Best heuristic value: 1 [g=129, 156 evaluated, 155 expanded, t=0.08s, 8596 KB]
Solution found!
Actual search time: 0.02s [t=0.08s]
anon-pickup_box-ugv1 ugv1store cdm1 area1 box1 (1)
anon-drop_box-ugv1 ugv1store area1 box1 (1)
anon-take_picture-usv3 area13 disaster7 (1)
anon-communicate_data-usv3 cdm2 disaster7 area13 area13 (1)
anon-take_picture-usv1 area1 disaster5 (1)
anon-communicate_data-usv1 cdm1 disaster5 area1 area1 (1)
anon-navigate_uav-uav1 area1 area17 (1)
anon-take_picture-uav1 area17 disaster8 (1)
anon-navigate_uav-uav1 area17 area9 (1)
anon-communicate_data-uav1 cdm2 disaster8 area9 area13 (1)
anon-navigate_uav-uav1 area9 area15 (1)
anon-take_picture-uav1 area15 disaster6 (1)
anon-navigate_uav-uav1 area15 area9 (1)
anon-communicate_data-uav1 cdm2 disaster6 area9 area13 (1)
anon-navigate_uav-uav1 area9 area10 (1)
anon-take_picture-uav1 area10 disaster4 (1)
anon-communicate_data-uav1 cdm2 disaster4 area10 area13 (1)
anon-navigate_uav-uav1 area10 area24 (1)
anon-take_picture-uav1 area24 disaster3 (1)
anon-navigate_uav-uav1 area24 area9 (1)
anon-communicate_data-uav1 cdm2 disaster3 area9 area13 (1)
anon-navigate_uav-uav1 area9 area23 (1)
anon-take_picture-uav1 area23 disaster2 (1)
anon-communicate_data-uav1 cdm3 disaster2 area23 area21 (1)
anon-navigate_uav-uav1 area23 area25 (1)
anon-take_picture-uav1 area25 disaster1 (1)
anon-navigate_uav-uav1 area25 area9 (1)
anon-communicate_data-uav1 cdm2 disaster1 area9 area13 (1)
anon-navigate_usv-usv5 area21 area22 (1)
anon-navigate_usv-usv5 area22 area26 (1)
anon-sample_water-usv5 usv5store area26 (1)
anon-navigate_usv-usv5 area26 area22 (1)
anon-navigate_usv-usv5 area22 area21 (1)
anon-drop_sample-usv5 usv5store area21 area26 cdm3 (1)
anon-navigate_usv-usv1 area1 area2 (1)
anon-navigate_usv-usv1 area2 area14 (1)
anon-sample_water-usv1 usv1store area14 (1)
anon-navigate_usv-usv1 area14 area2 (1)
anon-navigate_usv-usv1 area2 area1 (1)
anon-drop_sample-usv1 usv1store area1 area14 cdm1 (1)
anon-navigate_usv-usv1 area1 area2 (1)
anon-navigate_usv-usv1 area2 area6 (1)
anon-navigate_usv-usv1 area6 area12 (1)
anon-sample_water-usv1 usv1store area12 (1)
anon-navigate_usv-usv1 area12 area6 (1)
anon-navigate_usv-usv1 area6 area2 (1)
anon-navigate_usv-usv1 area2 area1 (1)
anon-drop_sample-usv1 usv1store area1 area12 cdm1 (1)
anon-pickup_box-ugv3 ugv3store cdm2 area13 box2 (1)
anon-navigate_ugv-ugv3 area13 area10 (1)
anon-navigate_ugv-ugv3 area10 area7 (1)
anon-navigate_ugv-ugv3 area7 area8 (1)
anon-navigate_ugv-ugv3 area8 area27 (1)
anon-drop_box-ugv3 ugv3store area27 box2 (1)
anon-pickup_box-ugv1 ugv1store cdm1 area1 box3 (1)
anon-navigate_ugv-ugv1 area1 area3 (1)
anon-navigate_ugv-ugv1 area3 area15 (1)
anon-navigate_ugv-ugv1 area15 area18 (1)
anon-navigate_ugv-ugv1 area18 area20 (1)
anon-drop_box-ugv1 ugv1store area20 box3 (1)
anon-navigate_usv-usv1 area1 area16 (1)
anon-navigate_usv-usv1 area16 area18 (1)
anon-sample_water-usv1 usv1store area18 (1)
anon-navigate_usv-usv1 area18 area16 (1)
anon-navigate_usv-usv1 area16 area14 (1)
anon-navigate_usv-usv1 area14 area2 (1)
anon-navigate_usv-usv1 area2 area4 (1)
anon-navigate_usv-usv1 area4 area9 (1)
anon-navigate_usv-usv1 area9 area13 (1)
anon-drop_sample-usv1 usv1store area13 area18 cdm2 (1)
anon-navigate_usv-usv5 area21 area22 (1)
anon-navigate_usv-usv5 area22 area24 (1)
anon-navigate_usv-usv5 area24 area29 (1)
anon-navigate_usv-usv5 area29 area28 (1)
anon-navigate_usv-usv5 area28 area7 (1)
anon-navigate_usv-usv5 area7 area8 (1)
anon-navigate_usv-usv5 area8 area9 (1)
anon-navigate_usv-usv5 area9 area4 (1)
anon-navigate_usv-usv5 area4 area2 (1)
anon-sample_water-usv5 usv5store area2 (1)
anon-navigate_usv-usv5 area2 area4 (1)
anon-navigate_usv-usv5 area4 area9 (1)
anon-navigate_usv-usv5 area9 area8 (1)
anon-navigate_usv-usv5 area8 area7 (1)
anon-navigate_usv-usv5 area7 area28 (1)
anon-navigate_usv-usv5 area28 area29 (1)
anon-navigate_usv-usv5 area29 area24 (1)
anon-navigate_usv-usv5 area24 area22 (1)
anon-navigate_usv-usv5 area22 area21 (1)
anon-drop_sample-usv5 usv5store area21 area2 cdm3 (1)
anon-navigate_ugv-ugv3 area27 area30 (1)
anon-navigate_ugv-ugv3 area30 area24 (1)
anon-navigate_ugv-ugv3 area24 area23 (1)
anon-navigate_ugv-ugv3 area23 area21 (1)
anon-pickup_box-ugv3 ugv3store cdm3 area21 box4 (1)
anon-navigate_ugv-ugv3 area21 area23 (1)
anon-navigate_ugv-ugv3 area23 area24 (1)
anon-navigate_ugv-ugv3 area24 area30 (1)
anon-navigate_ugv-ugv3 area30 area27 (1)
anon-navigate_ugv-ugv3 area27 area8 (1)
anon-navigate_ugv-ugv3 area8 area7 (1)
anon-navigate_ugv-ugv3 area7 area10 (1)
anon-navigate_ugv-ugv3 area10 area4 (1)
anon-navigate_ugv-ugv3 area4 area3 (1)
anon-navigate_ugv-ugv3 area3 area15 (1)
anon-drop_box-ugv3 ugv3store area15 box4 (1)
anon-navigate_ugv-ugv3 area15 area3 (1)
anon-navigate_ugv-ugv3 area3 area4 (1)
anon-navigate_ugv-ugv3 area4 area10 (1)
anon-navigate_ugv-ugv3 area10 area7 (1)
anon-navigate_ugv-ugv3 area7 area8 (1)
anon-navigate_ugv-ugv3 area8 area27 (1)
anon-navigate_ugv-ugv3 area27 area30 (1)
anon-navigate_ugv-ugv3 area30 area24 (1)
anon-navigate_ugv-ugv3 area24 area23 (1)
anon-navigate_ugv-ugv3 area23 area21 (1)
anon-pickup_box-ugv3 ugv3store cdm3 area21 box5 (1)
anon-navigate_ugv-ugv3 area21 area23 (1)
anon-navigate_ugv-ugv3 area23 area24 (1)
anon-navigate_ugv-ugv3 area24 area30 (1)
anon-navigate_ugv-ugv3 area30 area27 (1)
anon-navigate_ugv-ugv3 area27 area8 (1)
anon-navigate_ugv-ugv3 area8 area7 (1)
anon-navigate_ugv-ugv3 area7 area10 (1)
anon-navigate_ugv-ugv3 area10 area4 (1)
anon-navigate_ugv-ugv3 area4 area3 (1)
anon-navigate_ugv-ugv3 area3 area5 (1)
anon-navigate_ugv-ugv3 area5 area11 (1)
anon-navigate_ugv-ugv3 area11 area9 (1)
anon-drop_box-ugv3 ugv3store area9 box5 (1)
Plan length: 130 step(s).
Plan cost: 130
Initial state h value: 79.
Expanded 156 state(s).
Reopened 0 state(s).
Evaluated 157 state(s).
Evaluations: 314
Generated 16051 state(s).
Dead ends: 0 state(s).
Search time: 0.02s
Total time: 0.08s
Solution found.
Peak memory: 8596 KB
