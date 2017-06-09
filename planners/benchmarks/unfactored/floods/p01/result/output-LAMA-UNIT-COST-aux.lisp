INFO     Running translator.
INFO     translator inputs: ['/home/rovane/planners/benchmarks/unfactored/floods/p01/merged-obfuscated-domain.pddl', '/home/rovane/planners/benchmarks/unfactored/floods/p01/merged-obfuscated-problem.pddl']
INFO     translator arguments: []
Parsing...
Parsing: [0.000s CPU, 0.005s wall-clock]
Normalizing task... [0.000s CPU, 0.000s wall-clock]
Instantiating...
Generating Datalog program... [0.000s CPU, 0.001s wall-clock]
Normalizing Datalog program...
Normalizing Datalog program: [0.010s CPU, 0.006s wall-clock]
Preparing model... [0.000s CPU, 0.004s wall-clock]
Generated 141 rules.
Computing model... [0.020s CPU, 0.020s wall-clock]
897 relevant atoms
570 auxiliary atoms
1467 final queue length
1995 total queue pushes
Completing instantiation... [0.020s CPU, 0.021s wall-clock]
Instantiating: [0.050s CPU, 0.053s wall-clock]
Computing fact groups...
Finding invariants...
43 initial candidates
Finding invariants: [0.020s CPU, 0.012s wall-clock]
Checking invariant weight... [0.000s CPU, 0.000s wall-clock]
Instantiating groups... [0.000s CPU, 0.001s wall-clock]
Collecting mutex groups... [0.000s CPU, 0.000s wall-clock]
Choosing groups...
84 uncovered facts
Choosing groups: [0.000s CPU, 0.000s wall-clock]
Building translation key... [0.000s CPU, 0.001s wall-clock]
Computing fact groups: [0.030s CPU, 0.016s wall-clock]
Building STRIPS to SAS dictionary... [0.000s CPU, 0.000s wall-clock]
Building dictionary for full mutex groups... [0.000s CPU, 0.000s wall-clock]
Building mutex information...
Building mutex information: [0.000s CPU, 0.000s wall-clock]
Translating task...
Processing axioms...
Simplifying axioms... [0.000s CPU, 0.000s wall-clock]
Processing axioms: [0.000s CPU, 0.001s wall-clock]
Translating task: [0.020s CPU, 0.021s wall-clock]
282 effect conditions simplified
0 implied preconditions added
Detecting unreachable propositions...
0 operators removed
11 propositions removed
Detecting unreachable propositions: [0.000s CPU, 0.004s wall-clock]
Translator variables: 89
Translator derived variables: 0
Translator facts: 257
Translator goal facts: 9
Translator mutex groups: 11
Translator total mutex groups size: 101
Translator operators: 553
Translator axioms: 0
Translator task size: 2759
Translator peak memory: 30372 KB
Writing output... [0.010s CPU, 0.005s wall-clock]
Done! [0.110s CPU, 0.106s wall-clock]
INFO     Running preprocessor.
INFO     preprocessor input: output.sas
INFO     preprocessor arguments: []
Building causal graph...
The causal graph is not acyclic.
63 variables of 89 necessary
0 of 11 mutex groups necessary.
553 of 553 operators necessary.
0 of 0 axiom rules necessary.
Building domain transition graphs...
solveable in poly time 0
Building successor generator...
Preprocessor facts: 205
Preprocessor derived variables: 0
Preprocessor task size: 2412
Writing output...
done
INFO     Running search.
INFO     search input: output
INFO     search executable: /home/rovane/planners/cmap/planning/fd/src/search/downward-release
INFO     search arguments: ['--heuristic', 'hlm,hff=lm_ff_syn(lm_rhw(reasonable_orders=true,lm_cost_type=1,cost_type=1))', '--search', 'lazy_greedy(hff,hlm,preferred=[hff,hlm])', '--internal-plan-file', '/home/rovane/planners/benchmarks/unfactored/floods/p01/result/plan-LAMA-UNIT-COST.lisp']
reading input... [t=0s]
Simplifying transitions... done!
done reading input! [t=0s]
building causal graph...done! [t=0s]
packing state variables...done! [t=0s]
Variables: 63
Facts: 205
Bytes per state: 12
done initalizing global data [t=0s]
Initializing Exploration...
Generating landmarks using the RPG/SAS+ approach
approx. reasonable orders
approx. obedient reasonable orders
Removed 0 reasonable or obedient reasonable orders
Landmarks generation time: 0.00204813s
Discovered 18 landmarks, of which 0 are disjunctive and 0 are conjunctive 
9 edges
Initializing LAMA-FF Synergy Object
Initializing landmarks count heuristic...
9 initial landmarks, 9 goal landmarks
Conducting lazy best first search, (real) bound = 2147483647
Best heuristic value: 37 [g=0, 1 evaluated, 0 expanded, t=0s, 4496 KB]
Best heuristic value: 36 [g=1, 2 evaluated, 1 expanded, t=0s, 4496 KB]
Best heuristic value: 35 [g=2, 3 evaluated, 2 expanded, t=0s, 4496 KB]
Best heuristic value: 34 [g=3, 4 evaluated, 3 expanded, t=0s, 4496 KB]
Best heuristic value: 33 [g=5, 7 evaluated, 6 expanded, t=0s, 4496 KB]
Best heuristic value: 32 [g=6, 8 evaluated, 7 expanded, t=0s, 4496 KB]
Best heuristic value: 31 [g=7, 9 evaluated, 8 expanded, t=0s, 4496 KB]
Best heuristic value: 30 [g=8, 10 evaluated, 9 expanded, t=0s, 4496 KB]
Best heuristic value: 29 [g=9, 11 evaluated, 10 expanded, t=0s, 4496 KB]
Best heuristic value: 28 [g=10, 12 evaluated, 11 expanded, t=0s, 4496 KB]
Best heuristic value: 26 [g=13, 16 evaluated, 15 expanded, t=0s, 4496 KB]
Best heuristic value: 25 [g=14, 17 evaluated, 16 expanded, t=0s, 4496 KB]
Best heuristic value: 24 [g=16, 19 evaluated, 18 expanded, t=0s, 4496 KB]
Best heuristic value: 23 [g=17, 20 evaluated, 19 expanded, t=0s, 4496 KB]
Best heuristic value: 22 [g=18, 21 evaluated, 20 expanded, t=0s, 4496 KB]
Best heuristic value: 21 [g=21, 24 evaluated, 23 expanded, t=0s, 4496 KB]
Best heuristic value: 20 [g=22, 25 evaluated, 24 expanded, t=0s, 4496 KB]
Best heuristic value: 19 [g=23, 26 evaluated, 25 expanded, t=0s, 4496 KB]
Best heuristic value: 18 [g=24, 27 evaluated, 26 expanded, t=0s, 4496 KB]
Best heuristic value: 17 [g=28, 31 evaluated, 30 expanded, t=0s, 4496 KB]
Best heuristic value: 16 [g=29, 32 evaluated, 31 expanded, t=0s, 4496 KB]
Best heuristic value: 15 [g=30, 33 evaluated, 32 expanded, t=0s, 4496 KB]
Best heuristic value: 14 [g=31, 34 evaluated, 33 expanded, t=0.01s, 4496 KB]
Best heuristic value: 13 [g=32, 35 evaluated, 34 expanded, t=0.01s, 4496 KB]
Best heuristic value: 11 [g=33, 36 evaluated, 35 expanded, t=0.01s, 4496 KB]
Best heuristic value: 10 [g=35, 40 evaluated, 39 expanded, t=0.01s, 4496 KB]
Best heuristic value: 9 [g=36, 41 evaluated, 40 expanded, t=0.01s, 4496 KB]
Best heuristic value: 8 [g=37, 42 evaluated, 41 expanded, t=0.01s, 4496 KB]
Best heuristic value: 7 [g=38, 43 evaluated, 42 expanded, t=0.01s, 4496 KB]
Best heuristic value: 6 [g=39, 45 evaluated, 44 expanded, t=0.01s, 4496 KB]
Best heuristic value: 5 [g=40, 46 evaluated, 45 expanded, t=0.01s, 4628 KB]
Best heuristic value: 4 [g=41, 47 evaluated, 46 expanded, t=0.01s, 4628 KB]
Best heuristic value: 3 [g=42, 48 evaluated, 47 expanded, t=0.01s, 4628 KB]
Best heuristic value: 2 [g=43, 49 evaluated, 48 expanded, t=0.01s, 4628 KB]
Best heuristic value: 1 [g=44, 50 evaluated, 49 expanded, t=0.01s, 4628 KB]
Solution found!
Actual search time: 0.01s [t=0.01s]
anon-navigate_uav-uav2 area1 area3 (1)
anon-take_picture-uav2 area3 disaster5 (1)
anon-communicate_data-uav2 cdm1 disaster5 area3 area1 (1)
anon-navigate_uav-uav2 area3 area7 (1)
anon-take_picture-uav2 area7 disaster4 (1)
anon-navigate_uav-uav2 area7 area9 (1)
anon-communicate_data-uav2 cdm2 disaster4 area9 area13 (1)
anon-navigate_uav-uav2 area9 area10 (1)
anon-take_picture-uav2 area10 disaster3 (1)
anon-communicate_data-uav2 cdm2 disaster3 area10 area13 (1)
anon-navigate_uav-uav2 area10 area12 (1)
anon-take_picture-uav2 area12 disaster2 (1)
anon-navigate_uav-uav2 area12 area9 (1)
anon-communicate_data-uav2 cdm2 disaster2 area9 area13 (1)
anon-navigate_uav-uav2 area9 area8 (1)
anon-take_picture-uav2 area8 disaster1 (1)
anon-navigate_uav-uav2 area8 area9 (1)
anon-communicate_data-uav2 cdm2 disaster1 area9 area13 (1)
anon-navigate_usv-usv3 area13 area9 (1)
anon-navigate_usv-usv3 area9 area8 (1)
anon-sample_water-usv3 usv3store area8 (1)
anon-navigate_usv-usv3 area8 area9 (1)
anon-navigate_usv-usv3 area9 area13 (1)
anon-drop_sample-usv3 usv3store area13 area8 cdm2 (1)
anon-navigate_usv-usv2 area1 area2 (1)
anon-navigate_usv-usv2 area2 area4 (1)
anon-navigate_usv-usv2 area4 area9 (1)
anon-sample_water-usv2 usv2store area9 (1)
anon-navigate_usv-usv2 area9 area4 (1)
anon-navigate_usv-usv2 area4 area2 (1)
anon-navigate_usv-usv2 area2 area1 (1)
anon-drop_sample-usv2 usv2store area1 area9 cdm1 (1)
anon-pickup_box-ugv1 ugv1store cdm1 area1 box1 (1)
anon-navigate_ugv-ugv1 area1 area3 (1)
anon-navigate_ugv-ugv1 area3 area5 (1)
anon-navigate_ugv-ugv1 area5 area11 (1)
anon-navigate_ugv-ugv1 area11 area9 (1)
anon-drop_box-ugv1 ugv1store area9 box1 (1)
anon-pickup_box-ugv3 ugv3store cdm2 area13 box2 (1)
anon-navigate_ugv-ugv3 area13 area10 (1)
anon-navigate_ugv-ugv3 area10 area4 (1)
anon-navigate_ugv-ugv3 area4 area3 (1)
anon-navigate_ugv-ugv3 area3 area5 (1)
anon-navigate_ugv-ugv3 area5 area11 (1)
anon-drop_box-ugv3 ugv3store area11 box2 (1)
Plan length: 45 step(s).
Plan cost: 45
Initial state h value: 37.
Expanded 50 state(s).
Reopened 0 state(s).
Evaluated 51 state(s).
Evaluations: 102
Generated 1148 state(s).
Dead ends: 0 state(s).
Search time: 0.01s
Total time: 0.01s
Solution found.
Peak memory: 4628 KB
