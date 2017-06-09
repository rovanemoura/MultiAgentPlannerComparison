INFO     Running translator.
INFO     translator inputs: ['/home/rovane/planners/benchmarks/unfactored/floods/p03/merged-obfuscated-domain.pddl', '/home/rovane/planners/benchmarks/unfactored/floods/p03/merged-obfuscated-problem.pddl']
INFO     translator arguments: []
Parsing...
Parsing: [0.000s CPU, 0.005s wall-clock]
Normalizing task... [0.000s CPU, 0.000s wall-clock]
Instantiating...
Generating Datalog program... [0.000s CPU, 0.002s wall-clock]
Normalizing Datalog program...
Normalizing Datalog program: [0.020s CPU, 0.006s wall-clock]
Preparing model... [0.000s CPU, 0.005s wall-clock]
Generated 141 rules.
Computing model... [0.040s CPU, 0.041s wall-clock]
2093 relevant atoms
994 auxiliary atoms
3087 final queue length
4542 total queue pushes
Completing instantiation... [0.050s CPU, 0.052s wall-clock]
Instantiating: [0.110s CPU, 0.107s wall-clock]
Computing fact groups...
Finding invariants...
43 initial candidates
Finding invariants: [0.010s CPU, 0.012s wall-clock]
Checking invariant weight... [0.000s CPU, 0.000s wall-clock]
Instantiating groups... [0.000s CPU, 0.002s wall-clock]
Collecting mutex groups... [0.000s CPU, 0.000s wall-clock]
Choosing groups...
166 uncovered facts
Choosing groups: [0.000s CPU, 0.001s wall-clock]
Building translation key... [0.000s CPU, 0.001s wall-clock]
Computing fact groups: [0.020s CPU, 0.019s wall-clock]
Building STRIPS to SAS dictionary... [0.000s CPU, 0.000s wall-clock]
Building dictionary for full mutex groups... [0.000s CPU, 0.000s wall-clock]
Building mutex information...
Building mutex information: [0.000s CPU, 0.001s wall-clock]
Translating task...
Processing axioms...
Simplifying axioms... [0.000s CPU, 0.000s wall-clock]
Processing axioms: [0.000s CPU, 0.003s wall-clock]
Translating task: [0.050s CPU, 0.055s wall-clock]
751 effect conditions simplified
0 implied preconditions added
Detecting unreachable propositions...
0 operators removed
21 propositions removed
Detecting unreachable propositions: [0.020s CPU, 0.011s wall-clock]
Translator variables: 169
Translator derived variables: 0
Translator facts: 533
Translator goal facts: 15
Translator mutex groups: 17
Translator total mutex groups size: 229
Translator operators: 1430
Translator axioms: 0
Translator task size: 6974
Translator peak memory: 34724 KB
Writing output... [0.010s CPU, 0.013s wall-clock]
Done! [0.210s CPU, 0.215s wall-clock]
INFO     Running preprocessor.
INFO     preprocessor input: output.sas
INFO     preprocessor arguments: []
Building causal graph...
The causal graph is not acyclic.
103 variables of 169 necessary
0 of 17 mutex groups necessary.
1430 of 1430 operators necessary.
0 of 0 axiom rules necessary.
Building domain transition graphs...
solveable in poly time 0
Building successor generator...
Preprocessor facts: 401
Preprocessor derived variables: 0
Preprocessor task size: 6063
Writing output...
done
INFO     Running search.
INFO     search input: output
INFO     search executable: /home/rovane/planners/cmap/planning/fd/src/search/downward-release
INFO     search arguments: ['--heuristic', 'hlm,hff=lm_ff_syn(lm_rhw(reasonable_orders=true,lm_cost_type=1,cost_type=1))', '--search', 'lazy_greedy(hff,hlm,preferred=[hff,hlm])', '--internal-plan-file', '/home/rovane/planners/benchmarks/unfactored/floods/p03/result/plan-LAMA-UNIT-COST.lisp']
reading input... [t=0s]
Simplifying transitions... done!
done reading input! [t=0.01s]
building causal graph...done! [t=0.01s]
packing state variables...done! [t=0.01s]
Variables: 103
Facts: 401
Bytes per state: 20
done initalizing global data [t=0.01s]
Initializing Exploration...
Generating landmarks using the RPG/SAS+ approach
approx. reasonable orders
approx. obedient reasonable orders
Removed 0 reasonable or obedient reasonable orders
Landmarks generation time: 0.0065374s
Discovered 30 landmarks, of which 0 are disjunctive and 0 are conjunctive 
15 edges
Initializing LAMA-FF Synergy Object
Initializing landmarks count heuristic...
15 initial landmarks, 15 goal landmarks
Conducting lazy best first search, (real) bound = 2147483647
Best heuristic value: 79 [g=0, 1 evaluated, 0 expanded, t=0.02s, 5988 KB]
Best heuristic value: 76 [g=1, 3 evaluated, 2 expanded, t=0.02s, 5988 KB]
Best heuristic value: 75 [g=2, 4 evaluated, 3 expanded, t=0.02s, 5988 KB]
Best heuristic value: 74 [g=7, 10 evaluated, 9 expanded, t=0.02s, 5988 KB]
Best heuristic value: 73 [g=8, 11 evaluated, 10 expanded, t=0.02s, 5988 KB]
Best heuristic value: 72 [g=9, 12 evaluated, 11 expanded, t=0.02s, 5988 KB]
Best heuristic value: 71 [g=11, 14 evaluated, 13 expanded, t=0.02s, 5988 KB]
Best heuristic value: 70 [g=12, 15 evaluated, 14 expanded, t=0.02s, 5988 KB]
Best heuristic value: 69 [g=13, 16 evaluated, 15 expanded, t=0.02s, 5988 KB]
Best heuristic value: 68 [g=15, 18 evaluated, 17 expanded, t=0.02s, 5988 KB]
Best heuristic value: 67 [g=16, 19 evaluated, 18 expanded, t=0.02s, 5988 KB]
Best heuristic value: 66 [g=17, 20 evaluated, 19 expanded, t=0.02s, 5988 KB]
Best heuristic value: 65 [g=19, 22 evaluated, 21 expanded, t=0.02s, 5988 KB]
Best heuristic value: 64 [g=20, 23 evaluated, 22 expanded, t=0.02s, 5988 KB]
Best heuristic value: 63 [g=21, 24 evaluated, 23 expanded, t=0.02s, 5988 KB]
Best heuristic value: 62 [g=22, 25 evaluated, 24 expanded, t=0.02s, 5988 KB]
Best heuristic value: 60 [g=25, 29 evaluated, 28 expanded, t=0.02s, 5988 KB]
Best heuristic value: 59 [g=26, 30 evaluated, 29 expanded, t=0.02s, 5988 KB]
Best heuristic value: 58 [g=28, 32 evaluated, 31 expanded, t=0.02s, 5988 KB]
Best heuristic value: 57 [g=29, 33 evaluated, 32 expanded, t=0.02s, 5988 KB]
Best heuristic value: 56 [g=30, 34 evaluated, 33 expanded, t=0.02s, 5988 KB]
Best heuristic value: 51 [g=33, 37 evaluated, 36 expanded, t=0.02s, 5988 KB]
Best heuristic value: 50 [g=34, 38 evaluated, 37 expanded, t=0.02s, 5988 KB]
Best heuristic value: 49 [g=35, 39 evaluated, 38 expanded, t=0.02s, 5988 KB]
Best heuristic value: 48 [g=40, 45 evaluated, 44 expanded, t=0.02s, 5988 KB]
Best heuristic value: 47 [g=41, 46 evaluated, 45 expanded, t=0.02s, 5988 KB]
Best heuristic value: 46 [g=42, 47 evaluated, 46 expanded, t=0.02s, 5988 KB]
Best heuristic value: 45 [g=43, 48 evaluated, 47 expanded, t=0.02s, 5988 KB]
Best heuristic value: 44 [g=44, 49 evaluated, 48 expanded, t=0.02s, 5988 KB]
Best heuristic value: 43 [g=47, 55 evaluated, 54 expanded, t=0.02s, 5988 KB]
Best heuristic value: 42 [g=48, 56 evaluated, 55 expanded, t=0.02s, 5988 KB]
Best heuristic value: 41 [g=49, 57 evaluated, 56 expanded, t=0.02s, 5988 KB]
Best heuristic value: 39 [g=51, 59 evaluated, 58 expanded, t=0.03s, 5988 KB]
Best heuristic value: 38 [g=52, 60 evaluated, 59 expanded, t=0.03s, 5988 KB]
Best heuristic value: 37 [g=53, 61 evaluated, 60 expanded, t=0.03s, 5988 KB]
Best heuristic value: 36 [g=54, 62 evaluated, 61 expanded, t=0.03s, 5988 KB]
Best heuristic value: 35 [g=59, 72 evaluated, 71 expanded, t=0.03s, 5988 KB]
Best heuristic value: 34 [g=60, 73 evaluated, 72 expanded, t=0.03s, 5988 KB]
Best heuristic value: 33 [g=61, 74 evaluated, 73 expanded, t=0.03s, 5988 KB]
Best heuristic value: 32 [g=62, 75 evaluated, 74 expanded, t=0.03s, 5988 KB]
Best heuristic value: 31 [g=63, 76 evaluated, 75 expanded, t=0.03s, 5988 KB]
Best heuristic value: 30 [g=71, 87 evaluated, 86 expanded, t=0.03s, 5988 KB]
Best heuristic value: 29 [g=72, 88 evaluated, 87 expanded, t=0.03s, 5988 KB]
Best heuristic value: 28 [g=73, 89 evaluated, 88 expanded, t=0.03s, 5988 KB]
Best heuristic value: 27 [g=74, 90 evaluated, 89 expanded, t=0.03s, 5988 KB]
Best heuristic value: 26 [g=75, 91 evaluated, 90 expanded, t=0.03s, 5988 KB]
Best heuristic value: 25 [g=85, 102 evaluated, 101 expanded, t=0.03s, 5988 KB]
Best heuristic value: 24 [g=86, 103 evaluated, 102 expanded, t=0.03s, 5988 KB]
Best heuristic value: 23 [g=87, 104 evaluated, 103 expanded, t=0.03s, 5988 KB]
Best heuristic value: 22 [g=88, 105 evaluated, 104 expanded, t=0.03s, 5988 KB]
Best heuristic value: 21 [g=89, 106 evaluated, 105 expanded, t=0.03s, 5988 KB]
Best heuristic value: 20 [g=90, 107 evaluated, 106 expanded, t=0.03s, 5988 KB]
Best heuristic value: 19 [g=91, 108 evaluated, 107 expanded, t=0.03s, 5988 KB]
Best heuristic value: 18 [g=92, 109 evaluated, 108 expanded, t=0.03s, 5988 KB]
Best heuristic value: 17 [g=93, 110 evaluated, 109 expanded, t=0.03s, 5988 KB]
Best heuristic value: 16 [g=94, 111 evaluated, 110 expanded, t=0.03s, 5988 KB]
Best heuristic value: 15 [g=95, 112 evaluated, 111 expanded, t=0.03s, 5988 KB]
Best heuristic value: 14 [g=96, 113 evaluated, 112 expanded, t=0.03s, 5988 KB]
Best heuristic value: 13 [g=106, 133 evaluated, 132 expanded, t=0.03s, 6120 KB]
Best heuristic value: 12 [g=107, 134 evaluated, 133 expanded, t=0.03s, 6120 KB]
Best heuristic value: 11 [g=108, 135 evaluated, 134 expanded, t=0.03s, 6120 KB]
Best heuristic value: 10 [g=109, 136 evaluated, 135 expanded, t=0.03s, 6120 KB]
Best heuristic value: 9 [g=110, 137 evaluated, 136 expanded, t=0.03s, 6120 KB]
Best heuristic value: 8 [g=111, 138 evaluated, 137 expanded, t=0.03s, 6120 KB]
Best heuristic value: 7 [g=112, 139 evaluated, 138 expanded, t=0.03s, 6120 KB]
Best heuristic value: 6 [g=113, 140 evaluated, 139 expanded, t=0.03s, 6120 KB]
Best heuristic value: 5 [g=114, 141 evaluated, 140 expanded, t=0.03s, 6120 KB]
Best heuristic value: 4 [g=115, 142 evaluated, 141 expanded, t=0.03s, 6120 KB]
Best heuristic value: 3 [g=116, 143 evaluated, 142 expanded, t=0.03s, 6120 KB]
Best heuristic value: 2 [g=117, 144 evaluated, 143 expanded, t=0.03s, 6120 KB]
Best heuristic value: 1 [g=118, 145 evaluated, 144 expanded, t=0.03s, 6120 KB]
Solution found!
Actual search time: 0.01s [t=0.03s]
anon-pickup_box-ugv1 ugv1store cdm1 area1 box3 (1)
anon-navigate_ugv-ugv1 area1 area3 (1)
anon-drop_box-ugv1 ugv1store area3 box3 (1)
anon-navigate_uav-uav4 area13 area18 (1)
anon-take_picture-uav4 area18 disaster7 (1)
anon-navigate_uav-uav4 area18 area9 (1)
anon-communicate_data-uav4 cdm2 disaster7 area9 area13 (1)
anon-navigate_uav-uav4 area9 area6 (1)
anon-take_picture-uav4 area6 disaster6 (1)
anon-navigate_uav-uav4 area6 area9 (1)
anon-communicate_data-uav4 cdm2 disaster6 area9 area13 (1)
anon-navigate_uav-uav4 area9 area7 (1)
anon-take_picture-uav4 area7 disaster5 (1)
anon-navigate_uav-uav4 area7 area9 (1)
anon-communicate_data-uav4 cdm2 disaster5 area9 area13 (1)
anon-navigate_uav-uav4 area9 area5 (1)
anon-take_picture-uav4 area5 disaster4 (1)
anon-navigate_uav-uav4 area5 area9 (1)
anon-communicate_data-uav4 cdm2 disaster4 area9 area13 (1)
anon-navigate_uav-uav4 area9 area10 (1)
anon-take_picture-uav4 area10 disaster3 (1)
anon-communicate_data-uav4 cdm2 disaster3 area10 area13 (1)
anon-navigate_uav-uav4 area10 area24 (1)
anon-take_picture-uav4 area24 disaster2 (1)
anon-navigate_uav-uav4 area24 area9 (1)
anon-communicate_data-uav4 cdm2 disaster2 area9 area13 (1)
anon-navigate_uav-uav4 area9 area20 (1)
anon-take_picture-uav4 area20 disaster1 (1)
anon-navigate_uav-uav4 area20 area9 (1)
anon-communicate_data-uav4 cdm2 disaster1 area9 area13 (1)
anon-navigate_usv-usv2 area1 area2 (1)
anon-navigate_usv-usv2 area2 area4 (1)
anon-sample_water-usv2 usv2store area4 (1)
anon-navigate_usv-usv2 area4 area2 (1)
anon-navigate_usv-usv2 area2 area1 (1)
anon-drop_sample-usv2 usv2store area1 area4 cdm1 (1)
anon-navigate_ugv-ugv1 area3 area1 (1)
anon-pickup_box-ugv1 ugv1store cdm1 area1 box1 (1)
anon-navigate_ugv-ugv1 area1 area3 (1)
anon-navigate_ugv-ugv1 area3 area15 (1)
anon-navigate_ugv-ugv1 area15 area18 (1)
anon-navigate_ugv-ugv1 area18 area20 (1)
anon-drop_box-ugv1 ugv1store area20 box1 (1)
anon-navigate_usv-usv4 area13 area9 (1)
anon-navigate_usv-usv4 area9 area4 (1)
anon-navigate_usv-usv4 area4 area5 (1)
anon-sample_water-usv4 usv4store area5 (1)
anon-navigate_usv-usv4 area5 area4 (1)
anon-navigate_usv-usv4 area4 area9 (1)
anon-navigate_usv-usv4 area9 area13 (1)
anon-navigate_usv-usv4 area13 area24 (1)
anon-navigate_usv-usv4 area24 area22 (1)
anon-navigate_usv-usv4 area22 area21 (1)
anon-drop_sample-usv4 usv4store area21 area5 cdm3 (1)
anon-pickup_box-ugv3 ugv3store cdm2 area13 box2 (1)
anon-navigate_ugv-ugv3 area13 area10 (1)
anon-navigate_ugv-ugv3 area10 area4 (1)
anon-navigate_ugv-ugv3 area4 area3 (1)
anon-navigate_ugv-ugv3 area3 area15 (1)
anon-navigate_ugv-ugv3 area15 area16 (1)
anon-navigate_ugv-ugv3 area16 area17 (1)
anon-navigate_ugv-ugv3 area17 area14 (1)
anon-drop_box-ugv3 ugv3store area14 box2 (1)
anon-navigate_usv-usv2 area1 area2 (1)
anon-navigate_usv-usv2 area2 area6 (1)
anon-navigate_usv-usv2 area6 area12 (1)
anon-navigate_usv-usv2 area12 area10 (1)
anon-sample_water-usv2 usv2store area10 (1)
anon-navigate_usv-usv2 area10 area12 (1)
anon-navigate_usv-usv2 area12 area6 (1)
anon-navigate_usv-usv2 area6 area2 (1)
anon-navigate_usv-usv2 area2 area4 (1)
anon-navigate_usv-usv2 area4 area9 (1)
anon-navigate_usv-usv2 area9 area13 (1)
anon-drop_sample-usv2 usv2store area13 area10 cdm2 (1)
anon-navigate_ugv-ugv1 area20 area18 (1)
anon-navigate_ugv-ugv1 area18 area15 (1)
anon-navigate_ugv-ugv1 area15 area3 (1)
anon-navigate_ugv-ugv1 area3 area4 (1)
anon-navigate_ugv-ugv1 area4 area10 (1)
anon-navigate_ugv-ugv1 area10 area13 (1)
anon-navigate_ugv-ugv1 area13 area24 (1)
anon-navigate_ugv-ugv1 area24 area23 (1)
anon-navigate_ugv-ugv1 area23 area21 (1)
anon-pickup_box-ugv1 ugv1store cdm3 area21 box4 (1)
anon-navigate_ugv-ugv1 area21 area23 (1)
anon-navigate_ugv-ugv1 area23 area24 (1)
anon-navigate_ugv-ugv1 area24 area13 (1)
anon-navigate_ugv-ugv1 area13 area10 (1)
anon-navigate_ugv-ugv1 area10 area4 (1)
anon-navigate_ugv-ugv1 area4 area3 (1)
anon-navigate_ugv-ugv1 area3 area15 (1)
anon-navigate_ugv-ugv1 area15 area18 (1)
anon-navigate_ugv-ugv1 area18 area20 (1)
anon-navigate_ugv-ugv1 area20 area19 (1)
anon-drop_box-ugv1 ugv1store area19 box4 (1)
anon-navigate_usv-usv2 area13 area9 (1)
anon-navigate_usv-usv2 area9 area4 (1)
anon-navigate_usv-usv2 area4 area2 (1)
anon-navigate_usv-usv2 area2 area14 (1)
anon-navigate_usv-usv2 area14 area16 (1)
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
anon-navigate_usv-usv2 area9 area13 (1)
anon-navigate_usv-usv2 area13 area24 (1)
anon-navigate_usv-usv2 area24 area22 (1)
anon-navigate_usv-usv2 area22 area21 (1)
anon-drop_sample-usv2 usv2store area21 area17 cdm3 (1)
Plan length: 119 step(s).
Plan cost: 119
Initial state h value: 79.
Expanded 145 state(s).
Reopened 0 state(s).
Evaluated 146 state(s).
Evaluations: 292
Generated 5031 state(s).
Dead ends: 0 state(s).
Search time: 0.01s
Total time: 0.03s
Solution found.
Peak memory: 6120 KB
