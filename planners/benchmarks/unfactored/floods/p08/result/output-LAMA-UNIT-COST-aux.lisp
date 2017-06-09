INFO     Running translator.
INFO     translator inputs: ['/home/rovane/planners/benchmarks/unfactored/floods/p08/merged-obfuscated-domain.pddl', '/home/rovane/planners/benchmarks/unfactored/floods/p08/merged-obfuscated-problem.pddl']
INFO     translator arguments: []
Parsing...
Parsing: [0.010s CPU, 0.010s wall-clock]
Normalizing task... [0.000s CPU, 0.001s wall-clock]
Instantiating...
Generating Datalog program... [0.000s CPU, 0.003s wall-clock]
Normalizing Datalog program...
Normalizing Datalog program: [0.010s CPU, 0.011s wall-clock]
Preparing model... [0.010s CPU, 0.009s wall-clock]
Generated 267 rules.
Computing model... [0.190s CPU, 0.187s wall-clock]
9276 relevant atoms
3655 auxiliary atoms
12931 final queue length
21880 total queue pushes
Completing instantiation... [0.310s CPU, 0.310s wall-clock]
Instantiating: [0.520s CPU, 0.526s wall-clock]
Computing fact groups...
Finding invariants...
67 initial candidates
Finding invariants: [0.050s CPU, 0.042s wall-clock]
Checking invariant weight... [0.000s CPU, 0.001s wall-clock]
Instantiating groups... [0.010s CPU, 0.015s wall-clock]
Collecting mutex groups... [0.010s CPU, 0.001s wall-clock]
Choosing groups...
493 uncovered facts
Choosing groups: [0.000s CPU, 0.003s wall-clock]
Building translation key... [0.000s CPU, 0.003s wall-clock]
Computing fact groups: [0.080s CPU, 0.076s wall-clock]
Building STRIPS to SAS dictionary... [0.000s CPU, 0.001s wall-clock]
Building dictionary for full mutex groups... [0.000s CPU, 0.002s wall-clock]
Building mutex information...
Building mutex information: [0.000s CPU, 0.002s wall-clock]
Translating task...
Processing axioms...
Simplifying axioms... [0.000s CPU, 0.000s wall-clock]
Processing axioms: [0.020s CPU, 0.013s wall-clock]
Translating task: [0.330s CPU, 0.328s wall-clock]
4707 effect conditions simplified
0 implied preconditions added
Detecting unreachable propositions...
0 operators removed
42 propositions removed
Detecting unreachable propositions: [0.070s CPU, 0.063s wall-clock]
Translator variables: 499
Translator derived variables: 0
Translator facts: 1831
Translator goal facts: 30
Translator mutex groups: 63
Translator total mutex groups size: 959
Translator operators: 7343
Translator axioms: 0
Translator task size: 37012
Translator peak memory: 63076 KB
Writing output... [0.070s CPU, 0.067s wall-clock]
Done! [1.090s CPU, 1.092s wall-clock]
INFO     Running preprocessor.
INFO     preprocessor input: output.sas
INFO     preprocessor arguments: []
Building causal graph...
The causal graph is not acyclic.
275 variables of 499 necessary
0 of 63 mutex groups necessary.
7343 of 7343 operators necessary.
0 of 0 axiom rules necessary.
Building domain transition graphs...
solveable in poly time 0
Building successor generator...
Preprocessor facts: 1383
Preprocessor derived variables: 0
Preprocessor task size: 31853
Writing output...
done
INFO     Running search.
INFO     search input: output
INFO     search executable: /home/rovane/planners/cmap/planning/fd/src/search/downward-release
INFO     search arguments: ['--heuristic', 'hlm,hff=lm_ff_syn(lm_rhw(reasonable_orders=true,lm_cost_type=1,cost_type=1))', '--search', 'lazy_greedy(hff,hlm,preferred=[hff,hlm])', '--internal-plan-file', '/home/rovane/planners/benchmarks/unfactored/floods/p08/result/plan-LAMA-UNIT-COST.lisp']
reading input... [t=0s]
Simplifying transitions... done!
done reading input! [t=0.13s]
building causal graph...done! [t=0.13s]
packing state variables...done! [t=0.13s]
Variables: 275
Facts: 1383
Bytes per state: 48
done initalizing global data [t=0.13s]
Initializing Exploration...
Generating landmarks using the RPG/SAS+ approach
approx. reasonable orders
approx. obedient reasonable orders
Removed 0 reasonable or obedient reasonable orders
Landmarks generation time: 0.0522881s
Discovered 60 landmarks, of which 0 are disjunctive and 0 are conjunctive 
30 edges
Initializing LAMA-FF Synergy Object
Initializing landmarks count heuristic...
30 initial landmarks, 30 goal landmarks
Conducting lazy best first search, (real) bound = 2147483647
Best heuristic value: 129 [g=0, 1 evaluated, 0 expanded, t=0.19s, 18864 KB]
Best heuristic value: 128 [g=1, 2 evaluated, 1 expanded, t=0.19s, 18864 KB]
Best heuristic value: 127 [g=2, 3 evaluated, 2 expanded, t=0.19s, 18864 KB]
Best heuristic value: 126 [g=4, 6 evaluated, 5 expanded, t=0.19s, 18864 KB]
Best heuristic value: 125 [g=5, 7 evaluated, 6 expanded, t=0.19s, 18864 KB]
Best heuristic value: 124 [g=6, 8 evaluated, 7 expanded, t=0.19s, 18864 KB]
Best heuristic value: 123 [g=8, 10 evaluated, 9 expanded, t=0.19s, 18864 KB]
Best heuristic value: 122 [g=9, 11 evaluated, 10 expanded, t=0.19s, 18864 KB]
Best heuristic value: 121 [g=10, 12 evaluated, 11 expanded, t=0.19s, 18864 KB]
Best heuristic value: 120 [g=12, 14 evaluated, 13 expanded, t=0.19s, 18864 KB]
Best heuristic value: 119 [g=13, 15 evaluated, 14 expanded, t=0.19s, 18864 KB]
Best heuristic value: 118 [g=14, 16 evaluated, 15 expanded, t=0.19s, 18864 KB]
Best heuristic value: 117 [g=16, 18 evaluated, 17 expanded, t=0.19s, 18864 KB]
Best heuristic value: 116 [g=17, 19 evaluated, 18 expanded, t=0.19s, 18864 KB]
Best heuristic value: 115 [g=18, 20 evaluated, 19 expanded, t=0.19s, 18864 KB]
Best heuristic value: 114 [g=20, 22 evaluated, 21 expanded, t=0.19s, 18864 KB]
Best heuristic value: 113 [g=21, 23 evaluated, 22 expanded, t=0.19s, 18864 KB]
Best heuristic value: 112 [g=22, 24 evaluated, 23 expanded, t=0.19s, 18864 KB]
Best heuristic value: 111 [g=23, 25 evaluated, 24 expanded, t=0.19s, 18864 KB]
Best heuristic value: 110 [g=24, 26 evaluated, 25 expanded, t=0.2s, 18864 KB]
Best heuristic value: 109 [g=25, 27 evaluated, 26 expanded, t=0.2s, 18864 KB]
Best heuristic value: 108 [g=27, 30 evaluated, 29 expanded, t=0.2s, 18864 KB]
Best heuristic value: 107 [g=28, 31 evaluated, 30 expanded, t=0.2s, 18864 KB]
Best heuristic value: 106 [g=29, 32 evaluated, 31 expanded, t=0.2s, 18864 KB]
Best heuristic value: 105 [g=30, 33 evaluated, 32 expanded, t=0.2s, 18864 KB]
Best heuristic value: 104 [g=31, 34 evaluated, 33 expanded, t=0.2s, 18864 KB]
Best heuristic value: 103 [g=32, 35 evaluated, 34 expanded, t=0.2s, 18864 KB]
Best heuristic value: 101 [g=35, 39 evaluated, 38 expanded, t=0.2s, 18864 KB]
Best heuristic value: 100 [g=36, 40 evaluated, 39 expanded, t=0.2s, 18864 KB]
Best heuristic value: 98 [g=39, 43 evaluated, 42 expanded, t=0.2s, 18864 KB]
Best heuristic value: 97 [g=40, 44 evaluated, 43 expanded, t=0.2s, 18864 KB]
Best heuristic value: 96 [g=42, 46 evaluated, 45 expanded, t=0.2s, 18864 KB]
Best heuristic value: 95 [g=43, 47 evaluated, 46 expanded, t=0.2s, 18864 KB]
Best heuristic value: 94 [g=44, 48 evaluated, 47 expanded, t=0.2s, 18864 KB]
Best heuristic value: 93 [g=46, 50 evaluated, 49 expanded, t=0.2s, 18864 KB]
Best heuristic value: 92 [g=47, 51 evaluated, 50 expanded, t=0.2s, 18864 KB]
Best heuristic value: 91 [g=48, 52 evaluated, 51 expanded, t=0.2s, 18864 KB]
Best heuristic value: 90 [g=51, 55 evaluated, 54 expanded, t=0.2s, 18864 KB]
Best heuristic value: 89 [g=52, 56 evaluated, 55 expanded, t=0.2s, 18864 KB]
Best heuristic value: 88 [g=53, 57 evaluated, 56 expanded, t=0.2s, 18864 KB]
Best heuristic value: 87 [g=54, 58 evaluated, 57 expanded, t=0.2s, 18864 KB]
Best heuristic value: 86 [g=55, 60 evaluated, 59 expanded, t=0.2s, 18864 KB]
Best heuristic value: 85 [g=56, 61 evaluated, 60 expanded, t=0.2s, 18996 KB]
Best heuristic value: 84 [g=57, 62 evaluated, 61 expanded, t=0.2s, 18996 KB]
Best heuristic value: 83 [g=58, 63 evaluated, 62 expanded, t=0.21s, 18996 KB]
Best heuristic value: 82 [g=59, 65 evaluated, 64 expanded, t=0.21s, 18996 KB]
Best heuristic value: 81 [g=60, 66 evaluated, 65 expanded, t=0.21s, 18996 KB]
Best heuristic value: 80 [g=61, 67 evaluated, 66 expanded, t=0.21s, 18996 KB]
Best heuristic value: 79 [g=62, 68 evaluated, 67 expanded, t=0.21s, 18996 KB]
Best heuristic value: 78 [g=63, 69 evaluated, 68 expanded, t=0.21s, 18996 KB]
Best heuristic value: 77 [g=66, 74 evaluated, 73 expanded, t=0.21s, 18996 KB]
Best heuristic value: 76 [g=67, 75 evaluated, 74 expanded, t=0.21s, 18996 KB]
Best heuristic value: 75 [g=81, 99 evaluated, 98 expanded, t=0.22s, 18996 KB]
Best heuristic value: 74 [g=82, 100 evaluated, 99 expanded, t=0.22s, 18996 KB]
Best heuristic value: 71 [g=91, 141 evaluated, 140 expanded, t=0.23s, 18996 KB]
Best heuristic value: 70 [g=97, 153 evaluated, 152 expanded, t=0.23s, 18996 KB]
Best heuristic value: 69 [g=98, 154 evaluated, 153 expanded, t=0.23s, 18996 KB]
Best heuristic value: 68 [g=100, 157 evaluated, 156 expanded, t=0.23s, 19128 KB]
Best heuristic value: 67 [g=102, 159 evaluated, 158 expanded, t=0.24s, 19128 KB]
Best heuristic value: 66 [g=103, 160 evaluated, 159 expanded, t=0.24s, 19128 KB]
Best heuristic value: 65 [g=108, 165 evaluated, 164 expanded, t=0.24s, 19128 KB]
Best heuristic value: 58 [g=109, 167 evaluated, 166 expanded, t=0.24s, 19128 KB]
Best heuristic value: 57 [g=132, 197 evaluated, 196 expanded, t=0.24s, 19128 KB]
Best heuristic value: 51 [g=133, 198 evaluated, 197 expanded, t=0.24s, 19128 KB]
Best heuristic value: 49 [g=134, 199 evaluated, 198 expanded, t=0.24s, 19128 KB]
Best heuristic value: 47 [g=136, 201 evaluated, 200 expanded, t=0.24s, 19128 KB]
Best heuristic value: 45 [g=145, 228 evaluated, 227 expanded, t=0.25s, 19128 KB]
Best heuristic value: 44 [g=154, 247 evaluated, 246 expanded, t=0.26s, 19260 KB]
Best heuristic value: 43 [g=155, 248 evaluated, 247 expanded, t=0.26s, 19260 KB]
Best heuristic value: 42 [g=156, 249 evaluated, 248 expanded, t=0.26s, 19260 KB]
Best heuristic value: 41 [g=157, 250 evaluated, 249 expanded, t=0.26s, 19260 KB]
Best heuristic value: 40 [g=158, 251 evaluated, 250 expanded, t=0.26s, 19260 KB]
Best heuristic value: 39 [g=159, 252 evaluated, 251 expanded, t=0.26s, 19260 KB]
Best heuristic value: 38 [g=160, 253 evaluated, 252 expanded, t=0.26s, 19260 KB]
Best heuristic value: 37 [g=161, 254 evaluated, 253 expanded, t=0.26s, 19260 KB]
Best heuristic value: 36 [g=162, 255 evaluated, 254 expanded, t=0.26s, 19260 KB]
Best heuristic value: 35 [g=163, 256 evaluated, 255 expanded, t=0.26s, 19260 KB]
Best heuristic value: 34 [g=164, 257 evaluated, 256 expanded, t=0.26s, 19260 KB]
Best heuristic value: 33 [g=166, 260 evaluated, 259 expanded, t=0.26s, 19260 KB]
Best heuristic value: 32 [g=172, 278 evaluated, 277 expanded, t=0.26s, 19260 KB]
Best heuristic value: 31 [g=173, 279 evaluated, 278 expanded, t=0.27s, 19260 KB]
Best heuristic value: 30 [g=174, 280 evaluated, 279 expanded, t=0.27s, 19260 KB]
Best heuristic value: 29 [g=175, 281 evaluated, 280 expanded, t=0.27s, 19260 KB]
Best heuristic value: 28 [g=176, 282 evaluated, 281 expanded, t=0.27s, 19260 KB]
Best heuristic value: 27 [g=177, 283 evaluated, 282 expanded, t=0.27s, 19260 KB]
Best heuristic value: 26 [g=178, 284 evaluated, 283 expanded, t=0.27s, 19260 KB]
Best heuristic value: 25 [g=179, 285 evaluated, 284 expanded, t=0.27s, 19260 KB]
Best heuristic value: 24 [g=180, 286 evaluated, 285 expanded, t=0.27s, 19260 KB]
Best heuristic value: 23 [g=181, 287 evaluated, 286 expanded, t=0.27s, 19260 KB]
Best heuristic value: 22 [g=189, 313 evaluated, 312 expanded, t=0.28s, 19260 KB]
Best heuristic value: 21 [g=190, 314 evaluated, 313 expanded, t=0.28s, 19260 KB]
Best heuristic value: 20 [g=191, 315 evaluated, 314 expanded, t=0.28s, 19260 KB]
Best heuristic value: 19 [g=192, 316 evaluated, 315 expanded, t=0.28s, 19260 KB]
Best heuristic value: 18 [g=193, 317 evaluated, 316 expanded, t=0.28s, 19260 KB]
Best heuristic value: 17 [g=194, 318 evaluated, 317 expanded, t=0.28s, 19260 KB]
Best heuristic value: 16 [g=195, 319 evaluated, 318 expanded, t=0.28s, 19260 KB]
Best heuristic value: 15 [g=196, 320 evaluated, 319 expanded, t=0.28s, 19260 KB]
Best heuristic value: 14 [g=197, 321 evaluated, 320 expanded, t=0.28s, 19260 KB]
Best heuristic value: 13 [g=198, 322 evaluated, 321 expanded, t=0.28s, 19392 KB]
Best heuristic value: 12 [g=199, 323 evaluated, 322 expanded, t=0.28s, 19392 KB]
Best heuristic value: 11 [g=210, 334 evaluated, 333 expanded, t=0.28s, 19392 KB]
Best heuristic value: 10 [g=211, 335 evaluated, 334 expanded, t=0.28s, 19392 KB]
Best heuristic value: 9 [g=212, 336 evaluated, 335 expanded, t=0.28s, 19392 KB]
Best heuristic value: 8 [g=213, 337 evaluated, 336 expanded, t=0.28s, 19392 KB]
Best heuristic value: 7 [g=214, 338 evaluated, 337 expanded, t=0.28s, 19392 KB]
Best heuristic value: 6 [g=215, 339 evaluated, 338 expanded, t=0.28s, 19392 KB]
Best heuristic value: 5 [g=216, 340 evaluated, 339 expanded, t=0.28s, 19392 KB]
Best heuristic value: 4 [g=217, 341 evaluated, 340 expanded, t=0.28s, 19392 KB]
Best heuristic value: 3 [g=218, 342 evaluated, 341 expanded, t=0.28s, 19392 KB]
Best heuristic value: 2 [g=219, 343 evaluated, 342 expanded, t=0.28s, 19392 KB]
Best heuristic value: 1 [g=220, 344 evaluated, 343 expanded, t=0.28s, 19392 KB]
Solution found!
Actual search time: 0.09s [t=0.28s]
anon-pickup_box-ugv9 ugv9store cdm5 area41 box7 (1)
anon-drop_box-ugv9 ugv9store area41 box7 (1)
anon-navigate_uav-uav9 area41 area25 (1)
anon-take_picture-uav9 area25 disaster9 (1)
anon-navigate_uav-uav9 area25 area9 (1)
anon-communicate_data-uav9 cdm2 disaster9 area9 area13 (1)
anon-navigate_uav-uav9 area9 area26 (1)
anon-take_picture-uav9 area26 disaster8 (1)
anon-navigate_uav-uav9 area26 area9 (1)
anon-communicate_data-uav9 cdm2 disaster8 area9 area13 (1)
anon-navigate_uav-uav9 area9 area6 (1)
anon-take_picture-uav9 area6 disaster7 (1)
anon-navigate_uav-uav9 area6 area9 (1)
anon-communicate_data-uav9 cdm2 disaster7 area9 area13 (1)
anon-navigate_uav-uav9 area9 area5 (1)
anon-take_picture-uav9 area5 disaster6 (1)
anon-navigate_uav-uav9 area5 area9 (1)
anon-communicate_data-uav9 cdm2 disaster6 area9 area13 (1)
anon-navigate_uav-uav9 area9 area4 (1)
anon-take_picture-uav9 area4 disaster5 (1)
anon-navigate_uav-uav9 area4 area9 (1)
anon-communicate_data-uav9 cdm2 disaster5 area9 area13 (1)
anon-navigate_uav-uav9 area9 area2 (1)
anon-take_picture-uav9 area2 disaster4 (1)
anon-communicate_data-uav9 cdm1 disaster4 area2 area1 (1)
anon-navigate_uav-uav9 area2 area32 (1)
anon-take_picture-uav9 area32 disaster3 (1)
anon-navigate_uav-uav9 area32 area9 (1)
anon-communicate_data-uav9 cdm2 disaster3 area9 area13 (1)
anon-navigate_uav-uav9 area9 area22 (1)
anon-take_picture-uav9 area22 disaster2 (1)
anon-communicate_data-uav9 cdm3 disaster2 area22 area21 (1)
anon-navigate_uav-uav9 area22 area47 (1)
anon-take_picture-uav9 area47 disaster12 (1)
anon-navigate_uav-uav9 area47 area9 (1)
anon-communicate_data-uav9 cdm2 disaster12 area9 area13 (1)
anon-navigate_uav-uav9 area9 area34 (1)
anon-take_picture-uav9 area34 disaster11 (1)
anon-navigate_uav-uav9 area34 area9 (1)
anon-communicate_data-uav9 cdm2 disaster11 area9 area13 (1)
anon-navigate_uav-uav9 area9 area15 (1)
anon-take_picture-uav9 area15 disaster10 (1)
anon-navigate_uav-uav9 area15 area9 (1)
anon-communicate_data-uav9 cdm2 disaster10 area9 area13 (1)
anon-navigate_uav-uav9 area9 area36 (1)
anon-take_picture-uav9 area36 disaster1 (1)
anon-navigate_uav-uav9 area36 area9 (1)
anon-communicate_data-uav9 cdm2 disaster1 area9 area13 (1)
anon-navigate_usv-usv5 area21 area22 (1)
anon-navigate_usv-usv5 area22 area24 (1)
anon-sample_water-usv5 usv5store area24 (1)
anon-navigate_usv-usv5 area24 area22 (1)
anon-navigate_usv-usv5 area22 area21 (1)
anon-drop_sample-usv5 usv5store area21 area24 cdm3 (1)
anon-pickup_box-ugv5 ugv5store cdm3 area21 box5 (1)
anon-navigate_ugv-ugv5 area21 area23 (1)
anon-navigate_ugv-ugv5 area23 area25 (1)
anon-drop_box-ugv5 ugv5store area25 box5 (1)
anon-pickup_box-ugv1 ugv1store cdm1 area1 box3 (1)
anon-navigate_ugv-ugv1 area1 area16 (1)
anon-navigate_ugv-ugv1 area16 area17 (1)
anon-navigate_ugv-ugv1 area17 area14 (1)
anon-drop_box-ugv1 ugv1store area14 box3 (1)
anon-pickup_box-ugv7 ugv7store cdm4 area33 box4 (1)
anon-navigate_ugv-ugv7 area33 area49 (1)
anon-navigate_ugv-ugv7 area49 area13 (1)
anon-navigate_ugv-ugv7 area13 area10 (1)
anon-drop_box-ugv7 ugv7store area10 box4 (1)
anon-navigate_usv-usv7 area33 area29 (1)
anon-navigate_usv-usv7 area29 area28 (1)
anon-sample_water-usv7 usv7store area28 (1)
anon-navigate_usv-usv7 area28 area29 (1)
anon-navigate_usv-usv7 area29 area33 (1)
anon-navigate_usv-usv7 area33 area50 (1)
anon-navigate_usv-usv7 area50 area13 (1)
anon-drop_sample-usv7 usv7store area13 area28 cdm2 (1)
anon-sample_water-usv3 usv3store area13 (1)
anon-navigate_usv-usv3 area13 area9 (1)
anon-navigate_usv-usv3 area9 area4 (1)
anon-navigate_usv-usv3 area4 area2 (1)
anon-navigate_usv-usv3 area2 area1 (1)
anon-drop_sample-usv3 usv3store area1 area13 cdm1 (1)
anon-navigate_usv-usv7 area13 area50 (1)
anon-sample_water-usv7 usv7store area50 (1)
anon-navigate_usv-usv7 area50 area13 (1)
anon-navigate_usv-usv7 area13 area9 (1)
anon-navigate_usv-usv7 area9 area4 (1)
anon-navigate_usv-usv7 area4 area2 (1)
anon-navigate_usv-usv7 area2 area1 (1)
anon-drop_sample-usv7 usv7store area1 area50 cdm1 (1)
anon-pickup_box-ugv3 ugv3store cdm2 area13 box2 (1)
anon-navigate_ugv-ugv3 area13 area49 (1)
anon-navigate_ugv-ugv3 area49 area33 (1)
anon-navigate_ugv-ugv3 area33 area30 (1)
anon-navigate_ugv-ugv3 area30 area24 (1)
anon-navigate_ugv-ugv3 area24 area23 (1)
anon-navigate_ugv-ugv3 area23 area35 (1)
anon-drop_box-ugv3 ugv3store area35 box2 (1)
anon-navigate_usv-usv5 area21 area22 (1)
anon-navigate_usv-usv5 area22 area26 (1)
anon-navigate_usv-usv5 area26 area32 (1)
anon-sample_water-usv5 usv5store area32 (1)
anon-navigate_usv-usv5 area32 area26 (1)
anon-navigate_usv-usv5 area26 area22 (1)
anon-navigate_usv-usv5 area22 area24 (1)
anon-navigate_usv-usv5 area24 area29 (1)
anon-navigate_usv-usv5 area29 area33 (1)
anon-drop_sample-usv5 usv5store area33 area32 cdm4 (1)
anon-navigate_usv-usv5 area33 area29 (1)
anon-navigate_usv-usv5 area29 area28 (1)
anon-navigate_usv-usv5 area28 area27 (1)
anon-sample_water-usv5 usv5store area27 (1)
anon-navigate_usv-usv5 area27 area8 (1)
anon-navigate_usv-usv5 area8 area9 (1)
anon-navigate_usv-usv5 area9 area13 (1)
anon-navigate_usv-usv5 area13 area50 (1)
anon-navigate_usv-usv5 area50 area46 (1)
anon-navigate_usv-usv5 area46 area42 (1)
anon-navigate_usv-usv5 area42 area41 (1)
anon-drop_sample-usv5 usv5store area41 area27 cdm5 (1)
anon-navigate_ugv-ugv7 area10 area13 (1)
anon-navigate_ugv-ugv7 area13 area49 (1)
anon-navigate_ugv-ugv7 area49 area33 (1)
anon-pickup_box-ugv7 ugv7store cdm4 area33 box6 (1)
anon-navigate_ugv-ugv7 area33 area49 (1)
anon-navigate_ugv-ugv7 area49 area13 (1)
anon-navigate_ugv-ugv7 area13 area10 (1)
anon-navigate_ugv-ugv7 area10 area4 (1)
anon-navigate_ugv-ugv7 area4 area3 (1)
anon-navigate_ugv-ugv7 area3 area15 (1)
anon-navigate_ugv-ugv7 area15 area16 (1)
anon-drop_box-ugv7 ugv7store area16 box6 (1)
anon-sample_water-usv3 usv3store area1 (1)
anon-navigate_usv-usv3 area1 area2 (1)
anon-navigate_usv-usv3 area2 area4 (1)
anon-navigate_usv-usv3 area4 area9 (1)
anon-navigate_usv-usv3 area9 area13 (1)
anon-navigate_usv-usv3 area13 area50 (1)
anon-navigate_usv-usv3 area50 area46 (1)
anon-navigate_usv-usv3 area46 area42 (1)
anon-navigate_usv-usv3 area42 area41 (1)
anon-drop_sample-usv3 usv3store area41 area1 cdm5 (1)
anon-pickup_box-ugv9 ugv9store cdm5 area41 box9 (1)
anon-navigate_ugv-ugv9 area41 area43 (1)
anon-navigate_ugv-ugv9 area43 area45 (1)
anon-navigate_ugv-ugv9 area45 area49 (1)
anon-navigate_ugv-ugv9 area49 area33 (1)
anon-navigate_ugv-ugv9 area33 area30 (1)
anon-navigate_ugv-ugv9 area30 area24 (1)
anon-navigate_ugv-ugv9 area24 area23 (1)
anon-navigate_ugv-ugv9 area23 area21 (1)
anon-drop_box-ugv9 ugv9store area21 box9 (1)
anon-navigate_usv-usv7 area1 area2 (1)
anon-sample_water-usv7 usv7store area2 (1)
anon-navigate_usv-usv7 area2 area14 (1)
anon-navigate_usv-usv7 area14 area16 (1)
anon-navigate_usv-usv7 area16 area18 (1)
anon-navigate_usv-usv7 area18 area20 (1)
anon-navigate_usv-usv7 area20 area19 (1)
anon-navigate_usv-usv7 area19 area17 (1)
anon-navigate_usv-usv7 area17 area38 (1)
anon-navigate_usv-usv7 area38 area36 (1)
anon-navigate_usv-usv7 area36 area21 (1)
anon-drop_sample-usv7 usv7store area21 area2 cdm3 (1)
anon-navigate_ugv-ugv9 area21 area23 (1)
anon-navigate_ugv-ugv3 area35 area23 (1)
anon-navigate_ugv-ugv3 area23 area24 (1)
anon-navigate_ugv-ugv7 area16 area1 (1)
anon-navigate_ugv-ugv9 area23 area24 (1)
anon-navigate_ugv-ugv9 area24 area26 (1)
anon-pickup_box-ugv7 ugv7store cdm1 area1 box1 (1)
anon-navigate_ugv-ugv7 area1 area3 (1)
anon-navigate_ugv-ugv7 area3 area15 (1)
anon-navigate_ugv-ugv7 area15 area18 (1)
anon-navigate_ugv-ugv7 area18 area37 (1)
anon-navigate_ugv-ugv7 area37 area36 (1)
anon-navigate_ugv-ugv7 area36 area35 (1)
anon-navigate_ugv-ugv7 area35 area23 (1)
anon-navigate_ugv-ugv7 area23 area24 (1)
anon-navigate_ugv-ugv7 area24 area26 (1)
anon-drop_box-ugv7 ugv7store area26 box1 (1)
anon-navigate_ugv-ugv3 area24 area30 (1)
anon-navigate_ugv-ugv3 area30 area33 (1)
anon-navigate_ugv-ugv3 area33 area49 (1)
anon-navigate_ugv-ugv3 area49 area45 (1)
anon-navigate_ugv-ugv3 area45 area43 (1)
anon-navigate_ugv-ugv3 area43 area41 (1)
anon-navigate_ugv-ugv5 area25 area31 (1)
anon-pickup_box-ugv3 ugv3store cdm5 area41 box8 (1)
anon-navigate_ugv-ugv3 area41 area43 (1)
anon-navigate_ugv-ugv3 area43 area45 (1)
anon-navigate_ugv-ugv3 area45 area49 (1)
anon-navigate_ugv-ugv3 area49 area33 (1)
anon-navigate_ugv-ugv3 area33 area30 (1)
anon-navigate_ugv-ugv3 area30 area24 (1)
anon-navigate_ugv-ugv3 area24 area23 (1)
anon-navigate_ugv-ugv3 area23 area25 (1)
anon-navigate_ugv-ugv3 area25 area31 (1)
anon-drop_box-ugv3 ugv3store area31 box8 (1)
anon-navigate_usv-usv5 area41 area42 (1)
anon-navigate_usv-usv5 area42 area46 (1)
anon-navigate_usv-usv5 area46 area50 (1)
anon-navigate_usv-usv5 area50 area13 (1)
anon-navigate_usv-usv5 area13 area9 (1)
anon-navigate_usv-usv5 area9 area4 (1)
anon-navigate_usv-usv5 area4 area2 (1)
anon-navigate_usv-usv5 area2 area6 (1)
anon-navigate_usv-usv5 area6 area12 (1)
anon-navigate_usv-usv5 area12 area10 (1)
anon-sample_water-usv5 usv5store area10 (1)
anon-navigate_usv-usv5 area10 area12 (1)
anon-navigate_usv-usv5 area12 area6 (1)
anon-navigate_usv-usv5 area6 area2 (1)
anon-navigate_usv-usv5 area2 area4 (1)
anon-navigate_usv-usv5 area4 area9 (1)
anon-navigate_usv-usv5 area9 area13 (1)
anon-navigate_usv-usv5 area13 area50 (1)
anon-navigate_usv-usv5 area50 area46 (1)
anon-navigate_usv-usv5 area46 area42 (1)
anon-navigate_usv-usv5 area42 area41 (1)
anon-drop_sample-usv5 usv5store area41 area10 cdm5 (1)
Plan length: 221 step(s).
Plan cost: 221
Initial state h value: 129.
Expanded 344 state(s).
Reopened 0 state(s).
Evaluated 345 state(s).
Evaluations: 690
Generated 24393 state(s).
Dead ends: 0 state(s).
Search time: 0.09s
Total time: 0.28s
Solution found.
Peak memory: 19392 KB
