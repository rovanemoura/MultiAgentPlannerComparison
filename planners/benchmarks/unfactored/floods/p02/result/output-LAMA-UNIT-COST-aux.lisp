INFO     Running translator.
INFO     translator inputs: ['/home/rovane/planners/benchmarks/unfactored/floods/p02/merged-obfuscated-domain.pddl', '/home/rovane/planners/benchmarks/unfactored/floods/p02/merged-obfuscated-problem.pddl']
INFO     translator arguments: []
Parsing...
Parsing: [0.000s CPU, 0.004s wall-clock]
Normalizing task... [0.000s CPU, 0.000s wall-clock]
Instantiating...
Generating Datalog program... [0.010s CPU, 0.001s wall-clock]
Normalizing Datalog program...
Normalizing Datalog program: [0.000s CPU, 0.005s wall-clock]
Preparing model... [0.000s CPU, 0.004s wall-clock]
Generated 109 rules.
Computing model... [0.020s CPU, 0.024s wall-clock]
1256 relevant atoms
627 auxiliary atoms
1883 final queue length
2624 total queue pushes
Completing instantiation... [0.030s CPU, 0.028s wall-clock]
Instantiating: [0.060s CPU, 0.063s wall-clock]
Computing fact groups...
Finding invariants...
37 initial candidates
Finding invariants: [0.010s CPU, 0.009s wall-clock]
Checking invariant weight... [0.000s CPU, 0.000s wall-clock]
Instantiating groups... [0.000s CPU, 0.001s wall-clock]
Collecting mutex groups... [0.000s CPU, 0.000s wall-clock]
Choosing groups...
110 uncovered facts
Choosing groups: [0.000s CPU, 0.000s wall-clock]
Building translation key... [0.010s CPU, 0.001s wall-clock]
Computing fact groups: [0.020s CPU, 0.014s wall-clock]
Building STRIPS to SAS dictionary... [0.000s CPU, 0.000s wall-clock]
Building dictionary for full mutex groups... [0.000s CPU, 0.000s wall-clock]
Building mutex information...
Building mutex information: [0.000s CPU, 0.001s wall-clock]
Translating task...
Processing axioms...
Simplifying axioms... [0.000s CPU, 0.000s wall-clock]
Processing axioms: [0.000s CPU, 0.001s wall-clock]
Translating task: [0.030s CPU, 0.029s wall-clock]
348 effect conditions simplified
0 implied preconditions added
Detecting unreachable propositions...
0 operators removed
17 propositions removed
Detecting unreachable propositions: [0.000s CPU, 0.006s wall-clock]
Translator variables: 112
Translator derived variables: 0
Translator facts: 344
Translator goal facts: 12
Translator mutex groups: 10
Translator total mutex groups size: 140
Translator operators: 785
Translator axioms: 0
Translator task size: 3755
Translator peak memory: 31372 KB
Writing output... [0.010s CPU, 0.007s wall-clock]
Done! [0.120s CPU, 0.126s wall-clock]
INFO     Running preprocessor.
INFO     preprocessor input: output.sas
INFO     preprocessor arguments: []
Building causal graph...
The causal graph is not acyclic.
76 variables of 112 necessary
0 of 10 mutex groups necessary.
785 of 785 operators necessary.
0 of 0 axiom rules necessary.
Building domain transition graphs...
solveable in poly time 0
Building successor generator...
Preprocessor facts: 272
Preprocessor derived variables: 0
Preprocessor task size: 3276
Writing output...
done
INFO     Running search.
INFO     search input: output
INFO     search executable: /home/rovane/planners/cmap/planning/fd/src/search/downward-release
INFO     search arguments: ['--heuristic', 'hlm,hff=lm_ff_syn(lm_rhw(reasonable_orders=true,lm_cost_type=1,cost_type=1))', '--search', 'lazy_greedy(hff,hlm,preferred=[hff,hlm])', '--internal-plan-file', '/home/rovane/planners/benchmarks/unfactored/floods/p02/result/plan-LAMA-UNIT-COST.lisp']
reading input... [t=0s]
Simplifying transitions... done!
done reading input! [t=0s]
building causal graph...done! [t=0s]
packing state variables...done! [t=0s]
Variables: 76
Facts: 272
Bytes per state: 16
done initalizing global data [t=0s]
Initializing Exploration...
Generating landmarks using the RPG/SAS+ approach
approx. reasonable orders
approx. obedient reasonable orders
Removed 0 reasonable or obedient reasonable orders
Landmarks generation time: 0.00474295s
Discovered 38 landmarks, of which 0 are disjunctive and 0 are conjunctive 
61 edges
Initializing LAMA-FF Synergy Object
Initializing landmarks count heuristic...
14 initial landmarks, 12 goal landmarks
Conducting lazy best first search, (real) bound = 2147483647
Best heuristic value: 43 [g=0, 1 evaluated, 0 expanded, t=0.01s, 5012 KB]
Best heuristic value: 42 [g=2, 7 evaluated, 6 expanded, t=0.01s, 5012 KB]
Best heuristic value: 41 [g=5, 14 evaluated, 13 expanded, t=0.01s, 5012 KB]
Best heuristic value: 40 [g=7, 16 evaluated, 15 expanded, t=0.01s, 5012 KB]
Best heuristic value: 39 [g=8, 17 evaluated, 16 expanded, t=0.01s, 5012 KB]
Best heuristic value: 38 [g=9, 18 evaluated, 17 expanded, t=0.01s, 5012 KB]
Best heuristic value: 37 [g=11, 20 evaluated, 19 expanded, t=0.01s, 5012 KB]
Best heuristic value: 36 [g=12, 21 evaluated, 20 expanded, t=0.01s, 5012 KB]
Best heuristic value: 35 [g=13, 22 evaluated, 21 expanded, t=0.01s, 5012 KB]
Best heuristic value: 33 [g=16, 25 evaluated, 24 expanded, t=0.01s, 5012 KB]
Best heuristic value: 32 [g=17, 26 evaluated, 25 expanded, t=0.01s, 5012 KB]
Best heuristic value: 30 [g=20, 29 evaluated, 28 expanded, t=0.01s, 5012 KB]
Best heuristic value: 29 [g=21, 30 evaluated, 29 expanded, t=0.01s, 5012 KB]
Best heuristic value: 28 [g=23, 32 evaluated, 31 expanded, t=0.01s, 5012 KB]
Best heuristic value: 27 [g=24, 33 evaluated, 32 expanded, t=0.01s, 5012 KB]
Best heuristic value: 26 [g=25, 34 evaluated, 33 expanded, t=0.01s, 5012 KB]
Best heuristic value: 25 [g=26, 35 evaluated, 34 expanded, t=0.01s, 5012 KB]
Best heuristic value: 24 [g=27, 36 evaluated, 35 expanded, t=0.01s, 5012 KB]
Best heuristic value: 23 [g=28, 37 evaluated, 36 expanded, t=0.01s, 5012 KB]
Best heuristic value: 22 [g=29, 39 evaluated, 38 expanded, t=0.01s, 5012 KB]
Best heuristic value: 21 [g=31, 41 evaluated, 40 expanded, t=0.01s, 5012 KB]
Best heuristic value: 20 [g=32, 42 evaluated, 41 expanded, t=0.01s, 5012 KB]
Best heuristic value: 19 [g=39, 52 evaluated, 51 expanded, t=0.01s, 5012 KB]
Best heuristic value: 18 [g=44, 57 evaluated, 56 expanded, t=0.01s, 5012 KB]
Best heuristic value: 17 [g=45, 58 evaluated, 57 expanded, t=0.01s, 5012 KB]
Best heuristic value: 16 [g=49, 62 evaluated, 61 expanded, t=0.01s, 5012 KB]
Best heuristic value: 15 [g=50, 63 evaluated, 62 expanded, t=0.01s, 5012 KB]
Best heuristic value: 14 [g=51, 64 evaluated, 63 expanded, t=0.01s, 5012 KB]
Best heuristic value: 13 [g=52, 65 evaluated, 64 expanded, t=0.01s, 5012 KB]
Best heuristic value: 12 [g=53, 66 evaluated, 65 expanded, t=0.01s, 5012 KB]
Best heuristic value: 11 [g=54, 67 evaluated, 66 expanded, t=0.01s, 5012 KB]
Best heuristic value: 10 [g=55, 68 evaluated, 67 expanded, t=0.01s, 5012 KB]
Best heuristic value: 9 [g=56, 69 evaluated, 68 expanded, t=0.01s, 5012 KB]
Best heuristic value: 8 [g=55, 179 evaluated, 174 expanded, t=0.02s, 5012 KB]
Best heuristic value: 7 [g=56, 180 evaluated, 175 expanded, t=0.02s, 5012 KB]
Best heuristic value: 6 [g=57, 181 evaluated, 176 expanded, t=0.02s, 5012 KB]
Best heuristic value: 5 [g=58, 182 evaluated, 177 expanded, t=0.02s, 5012 KB]
Best heuristic value: 4 [g=60, 184 evaluated, 179 expanded, t=0.02s, 5012 KB]
Best heuristic value: 3 [g=61, 185 evaluated, 180 expanded, t=0.02s, 5012 KB]
Best heuristic value: 2 [g=62, 186 evaluated, 181 expanded, t=0.02s, 5012 KB]
Best heuristic value: 1 [g=63, 187 evaluated, 182 expanded, t=0.02s, 5012 KB]
Solution found!
Actual search time: 0.01s [t=0.02s]
anon-pickup_box-ugv1 ugv1store cdm1 area1 box3 (1)
anon-navigate_ugv-ugv1 area1 area3 (1)
anon-navigate_ugv-ugv1 area3 area5 (1)
anon-navigate_ugv-ugv1 area5 area11 (1)
anon-drop_box-ugv1 ugv1store area11 box3 (1)
anon-sample_water-usv3 usv3store area13 (1)
anon-drop_sample-usv3 usv3store area13 area13 cdm2 (1)
anon-navigate_usv-usv1 area1 area16 (1)
anon-sample_water-usv1 usv1store area16 (1)
anon-navigate_usv-usv1 area16 area1 (1)
anon-drop_sample-usv1 usv1store area1 area16 cdm1 (1)
anon-navigate_uav-uav3 area13 area7 (1)
anon-navigate_uav-uav3 area7 area9 (1)
anon-take_picture-uav3 area9 disaster5 (1)
anon-communicate_data-uav3 cdm2 disaster5 area9 area13 (1)
anon-navigate_uav-uav3 area9 area7 (1)
anon-take_picture-uav3 area7 disaster6 (1)
anon-navigate_uav-uav3 area7 area9 (1)
anon-communicate_data-uav3 cdm2 disaster6 area9 area13 (1)
anon-navigate_uav-uav3 area9 area5 (1)
anon-take_picture-uav3 area5 disaster4 (1)
anon-navigate_uav-uav3 area5 area9 (1)
anon-communicate_data-uav3 cdm2 disaster4 area9 area13 (1)
anon-navigate_uav-uav3 area9 area17 (1)
anon-take_picture-uav3 area17 disaster3 (1)
anon-navigate_uav-uav3 area17 area9 (1)
anon-communicate_data-uav3 cdm2 disaster3 area9 area13 (1)
anon-navigate_uav-uav3 area9 area2 (1)
anon-take_picture-uav3 area2 disaster2 (1)
anon-communicate_data-uav3 cdm1 disaster2 area2 area1 (1)
anon-take_picture-ugv1 area11 disaster1 (1)
anon-navigate_ugv-ugv1 area11 area9 (1)
anon-communicate_data-ugv1 cdm2 disaster1 area9 area13 (1)
anon-navigate_ugv-ugv1 area9 area11 (1)
anon-navigate_ugv-ugv1 area11 area5 (1)
anon-navigate_ugv-ugv1 area5 area3 (1)
anon-navigate_ugv-ugv1 area3 area4 (1)
anon-navigate_ugv-ugv1 area4 area10 (1)
anon-navigate_ugv-ugv1 area10 area13 (1)
anon-pickup_box-ugv1 ugv1store cdm2 area13 box2 (1)
anon-navigate_ugv-ugv1 area13 area10 (1)
anon-navigate_ugv-ugv1 area10 area4 (1)
anon-navigate_ugv-ugv1 area4 area3 (1)
anon-navigate_ugv-ugv1 area3 area15 (1)
anon-navigate_ugv-ugv1 area15 area18 (1)
anon-navigate_ugv-ugv1 area18 area20 (1)
anon-drop_box-ugv1 ugv1store area20 box2 (1)
anon-navigate_usv-usv3 area13 area9 (1)
anon-navigate_usv-usv3 area9 area4 (1)
anon-navigate_usv-usv3 area4 area5 (1)
anon-sample_water-usv3 usv3store area5 (1)
anon-navigate_usv-usv3 area5 area4 (1)
anon-navigate_usv-usv3 area4 area9 (1)
anon-navigate_usv-usv3 area9 area13 (1)
anon-drop_sample-usv3 usv3store area13 area5 cdm2 (1)
anon-navigate_ugv-ugv1 area20 area18 (1)
anon-navigate_ugv-ugv1 area18 area15 (1)
anon-navigate_ugv-ugv1 area15 area3 (1)
anon-navigate_ugv-ugv1 area3 area1 (1)
anon-pickup_box-ugv1 ugv1store cdm1 area1 box1 (1)
anon-navigate_ugv-ugv1 area1 area3 (1)
anon-navigate_ugv-ugv1 area3 area4 (1)
anon-navigate_ugv-ugv1 area4 area10 (1)
anon-drop_box-ugv1 ugv1store area10 box1 (1)
Plan length: 64 step(s).
Plan cost: 64
Initial state h value: 43.
Expanded 183 state(s).
Reopened 0 state(s).
Evaluated 188 state(s).
Evaluations: 376
Generated 5122 state(s).
Dead ends: 4 state(s).
Search time: 0.01s
Total time: 0.02s
Solution found.
Peak memory: 5012 KB
