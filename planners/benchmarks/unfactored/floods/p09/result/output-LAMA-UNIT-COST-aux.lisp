INFO     Running translator.
INFO     translator inputs: ['/home/rovane/planners/benchmarks/unfactored/floods/p09/merged-obfuscated-domain.pddl', '/home/rovane/planners/benchmarks/unfactored/floods/p09/merged-obfuscated-problem.pddl']
INFO     translator arguments: []
Parsing...
Parsing: [0.010s CPU, 0.011s wall-clock]
Normalizing task... [0.000s CPU, 0.001s wall-clock]
Instantiating...
Generating Datalog program... [0.010s CPU, 0.004s wall-clock]
Normalizing Datalog program...
Normalizing Datalog program: [0.010s CPU, 0.013s wall-clock]
Preparing model... [0.010s CPU, 0.011s wall-clock]
Generated 311 rules.
Computing model... [0.270s CPU, 0.274s wall-clock]
14939 relevant atoms
4750 auxiliary atoms
19689 final queue length
34214 total queue pushes
Completing instantiation... [0.530s CPU, 0.539s wall-clock]
Instantiating: [0.840s CPU, 0.846s wall-clock]
Computing fact groups...
Finding invariants...
77 initial candidates
Finding invariants: [0.070s CPU, 0.055s wall-clock]
Checking invariant weight... [0.000s CPU, 0.001s wall-clock]
Instantiating groups... [0.010s CPU, 0.018s wall-clock]
Collecting mutex groups... [0.000s CPU, 0.001s wall-clock]
Choosing groups...
692 uncovered facts
Choosing groups: [0.000s CPU, 0.004s wall-clock]
Building translation key... [0.000s CPU, 0.005s wall-clock]
Computing fact groups: [0.110s CPU, 0.101s wall-clock]
Building STRIPS to SAS dictionary... [0.000s CPU, 0.002s wall-clock]
Building dictionary for full mutex groups... [0.000s CPU, 0.002s wall-clock]
Building mutex information...
Building mutex information: [0.010s CPU, 0.002s wall-clock]
Translating task...
Processing axioms...
Simplifying axioms... [0.000s CPU, 0.000s wall-clock]
Processing axioms: [0.020s CPU, 0.023s wall-clock]
Translating task: [0.560s CPU, 0.564s wall-clock]
6958 effect conditions simplified
0 implied preconditions added
Detecting unreachable propositions...
0 operators removed
47 propositions removed
Detecting unreachable propositions: [0.100s CPU, 0.103s wall-clock]
Translator variables: 700
Translator derived variables: 0
Translator facts: 2430
Translator goal facts: 33
Translator mutex groups: 61
Translator total mutex groups size: 1152
Translator operators: 12496
Translator axioms: 0
Translator task size: 57959
Translator peak memory: 83888 KB
Writing output... [0.110s CPU, 0.113s wall-clock]
Done! [1.770s CPU, 1.773s wall-clock]
INFO     Running preprocessor.
INFO     preprocessor input: output.sas
INFO     preprocessor arguments: []
Building causal graph...
The causal graph is not acyclic.
413 variables of 700 necessary
0 of 61 mutex groups necessary.
12496 of 12496 operators necessary.
0 of 0 axiom rules necessary.
Building domain transition graphs...
solveable in poly time 0
Building successor generator...
Preprocessor facts: 1856
Preprocessor derived variables: 0
Preprocessor task size: 50916
Writing output...
done
INFO     Running search.
INFO     search input: output
INFO     search executable: /home/rovane/planners/cmap/planning/fd/src/search/downward-release
INFO     search arguments: ['--heuristic', 'hlm,hff=lm_ff_syn(lm_rhw(reasonable_orders=true,lm_cost_type=1,cost_type=1))', '--search', 'lazy_greedy(hff,hlm,preferred=[hff,hlm])', '--internal-plan-file', '/home/rovane/planners/benchmarks/unfactored/floods/p09/result/plan-LAMA-UNIT-COST.lisp']
reading input... [t=0s]
Simplifying transitions... done!
done reading input! [t=0.2s]
building causal graph...done! [t=0.2s]
packing state variables...done! [t=0.2s]
Variables: 413
Facts: 1856
Bytes per state: 68
done initalizing global data [t=0.2s]
Initializing Exploration...
Generating landmarks using the RPG/SAS+ approach
approx. reasonable orders
approx. obedient reasonable orders
Removed 0 reasonable or obedient reasonable orders
Landmarks generation time: 0.0958913s
Discovered 66 landmarks, of which 0 are disjunctive and 0 are conjunctive 
33 edges
Initializing LAMA-FF Synergy Object
Initializing landmarks count heuristic...
33 initial landmarks, 33 goal landmarks
Conducting lazy best first search, (real) bound = 2147483647
Best heuristic value: 160 [g=0, 1 evaluated, 0 expanded, t=0.3s, 24880 KB]
Best heuristic value: 158 [g=1, 2 evaluated, 1 expanded, t=0.3s, 24880 KB]
Best heuristic value: 157 [g=3, 4 evaluated, 3 expanded, t=0.3s, 24880 KB]
Best heuristic value: 156 [g=4, 5 evaluated, 4 expanded, t=0.3s, 24880 KB]
Best heuristic value: 155 [g=5, 6 evaluated, 5 expanded, t=0.3s, 24880 KB]
Best heuristic value: 154 [g=6, 7 evaluated, 6 expanded, t=0.31s, 24880 KB]
Best heuristic value: 152 [g=7, 9 evaluated, 8 expanded, t=0.31s, 24880 KB]
Best heuristic value: 150 [g=8, 10 evaluated, 9 expanded, t=0.31s, 24880 KB]
Best heuristic value: 149 [g=17, 19 evaluated, 18 expanded, t=0.31s, 24880 KB]
Best heuristic value: 148 [g=19, 21 evaluated, 20 expanded, t=0.31s, 24880 KB]
Best heuristic value: 147 [g=20, 22 evaluated, 21 expanded, t=0.32s, 24880 KB]
Best heuristic value: 146 [g=21, 23 evaluated, 22 expanded, t=0.32s, 24880 KB]
Best heuristic value: 145 [g=23, 25 evaluated, 24 expanded, t=0.32s, 24880 KB]
Best heuristic value: 144 [g=24, 26 evaluated, 25 expanded, t=0.32s, 24880 KB]
Best heuristic value: 143 [g=25, 27 evaluated, 26 expanded, t=0.32s, 24880 KB]
Best heuristic value: 142 [g=26, 28 evaluated, 27 expanded, t=0.32s, 24880 KB]
Best heuristic value: 141 [g=27, 29 evaluated, 28 expanded, t=0.32s, 24880 KB]
Best heuristic value: 140 [g=28, 30 evaluated, 29 expanded, t=0.32s, 24880 KB]
Best heuristic value: 139 [g=29, 31 evaluated, 30 expanded, t=0.32s, 24880 KB]
Best heuristic value: 138 [g=30, 32 evaluated, 31 expanded, t=0.32s, 24880 KB]
Best heuristic value: 137 [g=31, 33 evaluated, 32 expanded, t=0.32s, 24880 KB]
Best heuristic value: 136 [g=33, 36 evaluated, 35 expanded, t=0.32s, 24880 KB]
Best heuristic value: 135 [g=34, 37 evaluated, 36 expanded, t=0.32s, 24880 KB]
Best heuristic value: 134 [g=35, 38 evaluated, 37 expanded, t=0.32s, 24880 KB]
Best heuristic value: 133 [g=37, 40 evaluated, 39 expanded, t=0.32s, 24880 KB]
Best heuristic value: 132 [g=38, 41 evaluated, 40 expanded, t=0.32s, 24880 KB]
Best heuristic value: 131 [g=39, 42 evaluated, 41 expanded, t=0.32s, 24880 KB]
Best heuristic value: 130 [g=41, 44 evaluated, 43 expanded, t=0.32s, 24880 KB]
Best heuristic value: 129 [g=42, 45 evaluated, 44 expanded, t=0.32s, 24880 KB]
Best heuristic value: 128 [g=43, 46 evaluated, 45 expanded, t=0.32s, 24880 KB]
Best heuristic value: 127 [g=45, 48 evaluated, 47 expanded, t=0.32s, 24880 KB]
Best heuristic value: 126 [g=46, 49 evaluated, 48 expanded, t=0.32s, 24880 KB]
Best heuristic value: 125 [g=47, 50 evaluated, 49 expanded, t=0.32s, 24880 KB]
Best heuristic value: 124 [g=49, 52 evaluated, 51 expanded, t=0.32s, 24880 KB]
Best heuristic value: 123 [g=50, 53 evaluated, 52 expanded, t=0.32s, 24880 KB]
Best heuristic value: 122 [g=51, 54 evaluated, 53 expanded, t=0.33s, 24880 KB]
Best heuristic value: 119 [g=52, 55 evaluated, 54 expanded, t=0.33s, 24880 KB]
Best heuristic value: 116 [g=54, 57 evaluated, 56 expanded, t=0.33s, 24880 KB]
Best heuristic value: 115 [g=55, 65 evaluated, 64 expanded, t=0.33s, 24880 KB]
Best heuristic value: 114 [g=56, 66 evaluated, 65 expanded, t=0.33s, 24880 KB]
Best heuristic value: 113 [g=57, 68 evaluated, 67 expanded, t=0.33s, 24880 KB]
Best heuristic value: 112 [g=58, 69 evaluated, 68 expanded, t=0.34s, 24880 KB]
Best heuristic value: 111 [g=59, 70 evaluated, 69 expanded, t=0.34s, 24880 KB]
Best heuristic value: 110 [g=60, 71 evaluated, 70 expanded, t=0.34s, 24880 KB]
Best heuristic value: 109 [g=61, 72 evaluated, 71 expanded, t=0.34s, 24880 KB]
Best heuristic value: 108 [g=65, 76 evaluated, 75 expanded, t=0.34s, 24880 KB]
Best heuristic value: 107 [g=66, 77 evaluated, 76 expanded, t=0.34s, 24880 KB]
Best heuristic value: 106 [g=67, 78 evaluated, 77 expanded, t=0.34s, 24880 KB]
Best heuristic value: 105 [g=68, 79 evaluated, 78 expanded, t=0.34s, 24880 KB]
Best heuristic value: 104 [g=69, 80 evaluated, 79 expanded, t=0.34s, 24880 KB]
Best heuristic value: 103 [g=70, 81 evaluated, 80 expanded, t=0.34s, 24880 KB]
Best heuristic value: 102 [g=77, 90 evaluated, 89 expanded, t=0.34s, 24880 KB]
Best heuristic value: 101 [g=78, 91 evaluated, 90 expanded, t=0.34s, 24880 KB]
Best heuristic value: 100 [g=79, 92 evaluated, 91 expanded, t=0.34s, 24880 KB]
Best heuristic value: 99 [g=83, 102 evaluated, 101 expanded, t=0.35s, 24880 KB]
Best heuristic value: 96 [g=89, 109 evaluated, 108 expanded, t=0.35s, 24880 KB]
Best heuristic value: 95 [g=100, 121 evaluated, 120 expanded, t=0.36s, 24880 KB]
Best heuristic value: 94 [g=102, 124 evaluated, 123 expanded, t=0.36s, 24880 KB]
Best heuristic value: 86 [g=102, 128 evaluated, 127 expanded, t=0.36s, 24880 KB]
Best heuristic value: 85 [g=112, 144 evaluated, 143 expanded, t=0.37s, 24880 KB]
Best heuristic value: 84 [g=113, 145 evaluated, 144 expanded, t=0.37s, 24880 KB]
Best heuristic value: 79 [g=117, 155 evaluated, 154 expanded, t=0.38s, 24880 KB]
Best heuristic value: 78 [g=119, 158 evaluated, 157 expanded, t=0.38s, 24880 KB]
Best heuristic value: 74 [g=120, 159 evaluated, 158 expanded, t=0.38s, 24880 KB]
Best heuristic value: 72 [g=129, 175 evaluated, 174 expanded, t=0.38s, 24880 KB]
Best heuristic value: 71 [g=140, 186 evaluated, 185 expanded, t=0.39s, 24880 KB]
Best heuristic value: 70 [g=154, 221 evaluated, 220 expanded, t=0.4s, 24912 KB]
Best heuristic value: 69 [g=173, 261 evaluated, 260 expanded, t=0.42s, 24912 KB]
Best heuristic value: 68 [g=174, 262 evaluated, 261 expanded, t=0.42s, 25044 KB]
Best heuristic value: 67 [g=175, 263 evaluated, 262 expanded, t=0.42s, 25044 KB]
Best heuristic value: 58 [g=176, 264 evaluated, 263 expanded, t=0.42s, 25044 KB]
Best heuristic value: 53 [g=185, 276 evaluated, 275 expanded, t=0.43s, 25044 KB]
Best heuristic value: 52 [g=190, 296 evaluated, 295 expanded, t=0.44s, 25044 KB]
Best heuristic value: 51 [g=191, 297 evaluated, 296 expanded, t=0.44s, 25044 KB]
Best heuristic value: 50 [g=192, 298 evaluated, 297 expanded, t=0.44s, 25044 KB]
Best heuristic value: 49 [g=203, 314 evaluated, 313 expanded, t=0.44s, 25044 KB]
Best heuristic value: 48 [g=204, 315 evaluated, 314 expanded, t=0.44s, 25176 KB]
Best heuristic value: 47 [g=205, 316 evaluated, 315 expanded, t=0.44s, 25176 KB]
Best heuristic value: 46 [g=206, 317 evaluated, 316 expanded, t=0.45s, 25176 KB]
Best heuristic value: 45 [g=215, 327 evaluated, 326 expanded, t=0.45s, 25176 KB]
Best heuristic value: 44 [g=216, 328 evaluated, 327 expanded, t=0.45s, 25176 KB]
Best heuristic value: 43 [g=217, 329 evaluated, 328 expanded, t=0.45s, 25176 KB]
Best heuristic value: 42 [g=218, 330 evaluated, 329 expanded, t=0.45s, 25176 KB]
Best heuristic value: 41 [g=219, 331 evaluated, 330 expanded, t=0.45s, 25176 KB]
Best heuristic value: 40 [g=220, 332 evaluated, 331 expanded, t=0.45s, 25176 KB]
Best heuristic value: 39 [g=221, 333 evaluated, 332 expanded, t=0.45s, 25176 KB]
Best heuristic value: 38 [g=222, 334 evaluated, 333 expanded, t=0.45s, 25176 KB]
Best heuristic value: 37 [g=223, 335 evaluated, 334 expanded, t=0.46s, 25176 KB]
Best heuristic value: 36 [g=224, 336 evaluated, 335 expanded, t=0.46s, 25176 KB]
Best heuristic value: 35 [g=225, 337 evaluated, 336 expanded, t=0.46s, 25176 KB]
Best heuristic value: 34 [g=226, 338 evaluated, 337 expanded, t=0.46s, 25176 KB]
Best heuristic value: 28 [g=245, 364 evaluated, 363 expanded, t=0.47s, 25176 KB]
Best heuristic value: 27 [g=246, 365 evaluated, 364 expanded, t=0.47s, 25176 KB]
Best heuristic value: 25 [g=258, 385 evaluated, 384 expanded, t=0.48s, 25308 KB]
Best heuristic value: 22 [g=259, 386 evaluated, 385 expanded, t=0.48s, 25308 KB]
Best heuristic value: 21 [g=260, 387 evaluated, 386 expanded, t=0.48s, 25308 KB]
Best heuristic value: 20 [g=263, 390 evaluated, 389 expanded, t=0.48s, 25308 KB]
Best heuristic value: 19 [g=267, 394 evaluated, 393 expanded, t=0.48s, 25308 KB]
Best heuristic value: 17 [g=282, 420 evaluated, 419 expanded, t=0.49s, 25308 KB]
Best heuristic value: 16 [g=285, 423 evaluated, 422 expanded, t=0.49s, 25440 KB]
Best heuristic value: 15 [g=289, 427 evaluated, 426 expanded, t=0.49s, 25440 KB]
Best heuristic value: 14 [g=290, 428 evaluated, 427 expanded, t=0.49s, 25440 KB]
Best heuristic value: 13 [g=295, 442 evaluated, 441 expanded, t=0.5s, 25440 KB]
Best heuristic value: 12 [g=296, 443 evaluated, 442 expanded, t=0.5s, 25440 KB]
Best heuristic value: 11 [g=297, 444 evaluated, 443 expanded, t=0.5s, 25440 KB]
Best heuristic value: 10 [g=298, 445 evaluated, 444 expanded, t=0.5s, 25440 KB]
Best heuristic value: 9 [g=299, 446 evaluated, 445 expanded, t=0.5s, 25440 KB]
Best heuristic value: 8 [g=300, 447 evaluated, 446 expanded, t=0.5s, 25440 KB]
Best heuristic value: 7 [g=301, 448 evaluated, 447 expanded, t=0.5s, 25440 KB]
Best heuristic value: 6 [g=302, 449 evaluated, 448 expanded, t=0.5s, 25440 KB]
Best heuristic value: 5 [g=303, 450 evaluated, 449 expanded, t=0.5s, 25440 KB]
Best heuristic value: 4 [g=304, 451 evaluated, 450 expanded, t=0.5s, 25440 KB]
Best heuristic value: 3 [g=305, 452 evaluated, 451 expanded, t=0.5s, 25440 KB]
Best heuristic value: 2 [g=306, 453 evaluated, 452 expanded, t=0.5s, 25440 KB]
Best heuristic value: 1 [g=307, 454 evaluated, 453 expanded, t=0.5s, 25440 KB]
Solution found!
Actual search time: 0.2s [t=0.5s]
anon-pickup_box-ugv7 ugv7store cdm4 area33 box6 (1)
anon-drop_box-ugv7 ugv7store area33 box6 (1)
anon-take_picture-usv11 area53 disaster6 (1)
anon-communicate_data-usv11 cdm6 disaster6 area53 area53 (1)
anon-take_picture-usv6 area21 disaster3 (1)
anon-communicate_data-usv6 cdm3 disaster3 area21 area21 (1)
anon-pickup_box-ugv11 ugv11store cdm6 area53 box9 (1)
anon-navigate_ugv-ugv11 area53 area13 (1)
anon-drop_box-ugv11 ugv11store area13 box9 (1)
anon-navigate_uav-uav10 area41 area55 (1)
anon-navigate_uav-uav10 area55 area9 (1)
anon-take_picture-uav10 area9 disaster2 (1)
anon-communicate_data-uav10 cdm2 disaster2 area9 area13 (1)
anon-navigate_uav-uav10 area9 area55 (1)
anon-take_picture-uav10 area55 disaster9 (1)
anon-navigate_uav-uav10 area55 area9 (1)
anon-communicate_data-uav10 cdm2 disaster9 area9 area13 (1)
anon-navigate_uav-uav10 area9 area32 (1)
anon-take_picture-uav10 area32 disaster8 (1)
anon-navigate_uav-uav10 area32 area9 (1)
anon-communicate_data-uav10 cdm2 disaster8 area9 area13 (1)
anon-navigate_uav-uav10 area9 area18 (1)
anon-take_picture-uav10 area18 disaster7 (1)
anon-navigate_uav-uav10 area18 area9 (1)
anon-communicate_data-uav10 cdm2 disaster7 area9 area13 (1)
anon-navigate_uav-uav10 area9 area3 (1)
anon-take_picture-uav10 area3 disaster5 (1)
anon-communicate_data-uav10 cdm1 disaster5 area3 area1 (1)
anon-navigate_uav-uav10 area3 area22 (1)
anon-take_picture-uav10 area22 disaster4 (1)
anon-communicate_data-uav10 cdm3 disaster4 area22 area21 (1)
anon-navigate_uav-uav10 area22 area38 (1)
anon-take_picture-uav10 area38 disaster13 (1)
anon-navigate_uav-uav10 area38 area9 (1)
anon-communicate_data-uav10 cdm2 disaster13 area9 area13 (1)
anon-navigate_uav-uav10 area9 area36 (1)
anon-take_picture-uav10 area36 disaster12 (1)
anon-navigate_uav-uav10 area36 area9 (1)
anon-communicate_data-uav10 cdm2 disaster12 area9 area13 (1)
anon-navigate_uav-uav10 area9 area16 (1)
anon-take_picture-uav10 area16 disaster11 (1)
anon-navigate_uav-uav10 area16 area9 (1)
anon-communicate_data-uav10 cdm2 disaster11 area9 area13 (1)
anon-navigate_uav-uav10 area9 area8 (1)
anon-take_picture-uav10 area8 disaster10 (1)
anon-navigate_uav-uav10 area8 area9 (1)
anon-communicate_data-uav10 cdm2 disaster10 area9 area13 (1)
anon-navigate_uav-uav10 area9 area11 (1)
anon-take_picture-uav10 area11 disaster1 (1)
anon-navigate_uav-uav10 area11 area9 (1)
anon-communicate_data-uav10 cdm2 disaster1 area9 area13 (1)
anon-pickup_box-ugv3 ugv3store cdm2 area13 box2 (1)
anon-navigate_ugv-ugv3 area13 area53 (1)
anon-navigate_ugv-ugv3 area53 area50 (1)
anon-navigate_ugv-ugv3 area50 area44 (1)
anon-drop_box-ugv3 ugv3store area44 box2 (1)
anon-pickup_box-ugv9 ugv9store cdm5 area41 box8 (1)
anon-navigate_ugv-ugv9 area41 area43 (1)
anon-navigate_ugv-ugv9 area43 area45 (1)
anon-navigate_ugv-ugv9 area45 area51 (1)
anon-drop_box-ugv9 ugv9store area51 box8 (1)
anon-navigate_usv-usv11 area53 area49 (1)
anon-navigate_usv-usv11 area49 area44 (1)
anon-navigate_usv-usv11 area44 area42 (1)
anon-navigate_usv-usv11 area42 area54 (1)
anon-sample_water-usv11 usv11store area54 (1)
anon-navigate_usv-usv11 area54 area42 (1)
anon-navigate_usv-usv11 area42 area44 (1)
anon-navigate_usv-usv11 area44 area49 (1)
anon-navigate_usv-usv11 area49 area53 (1)
anon-drop_sample-usv11 usv11store area53 area54 cdm6 (1)
anon-navigate_ugv-ugv3 area44 area50 (1)
anon-navigate_ugv-ugv3 area50 area53 (1)
anon-pickup_box-ugv3 ugv3store cdm6 area53 box10 (1)
anon-navigate_ugv-ugv3 area53 area50 (1)
anon-navigate_ugv-ugv3 area50 area44 (1)
anon-navigate_ugv-ugv3 area44 area43 (1)
anon-navigate_ugv-ugv3 area43 area55 (1)
anon-drop_box-ugv3 ugv3store area55 box10 (1)
anon-navigate_ugv-ugv11 area13 area53 (1)
anon-navigate_ugv-ugv11 area53 area33 (1)
anon-pickup_box-ugv7 ugv7store cdm4 area33 box4 (1)
anon-navigate_ugv-ugv7 area33 area53 (1)
anon-navigate_ugv-ugv7 area53 area13 (1)
anon-navigate_ugv-ugv7 area13 area10 (1)
anon-navigate_ugv-ugv7 area10 area4 (1)
anon-navigate_ugv-ugv7 area4 area3 (1)
anon-drop_box-ugv7 ugv7store area3 box4 (1)
anon-navigate_ugv-ugv11 area33 area30 (1)
anon-navigate_ugv-ugv11 area30 area24 (1)
anon-navigate_ugv-ugv11 area24 area23 (1)
anon-navigate_ugv-ugv11 area23 area21 (1)
anon-pickup_box-ugv11 ugv11store cdm3 area21 box5 (1)
anon-navigate_ugv-ugv11 area21 area23 (1)
anon-navigate_ugv-ugv11 area23 area24 (1)
anon-navigate_ugv-ugv11 area24 area30 (1)
anon-navigate_ugv-ugv11 area30 area33 (1)
anon-navigate_ugv-ugv11 area33 area53 (1)
anon-navigate_ugv-ugv11 area53 area50 (1)
anon-drop_box-ugv11 ugv11store area50 box5 (1)
anon-navigate_usv-usv4 area13 area53 (1)
anon-navigate_usv-usv4 area53 area49 (1)
anon-navigate_usv-usv11 area53 area49 (1)
anon-navigate_usv-usv11 area49 area48 (1)
anon-navigate_usv-usv4 area49 area48 (1)
anon-sample_water-usv4 usv4store area48 (1)
anon-navigate_usv-usv4 area48 area49 (1)
anon-navigate_usv-usv4 area49 area53 (1)
anon-navigate_usv-usv4 area53 area13 (1)
anon-navigate_usv-usv4 area13 area9 (1)
anon-navigate_usv-usv4 area9 area4 (1)
anon-navigate_usv-usv4 area4 area2 (1)
anon-navigate_usv-usv4 area2 area1 (1)
anon-drop_sample-usv4 usv4store area1 area48 cdm1 (1)
anon-navigate_usv-usv6 area21 area36 (1)
anon-navigate_usv-usv6 area36 area38 (1)
anon-navigate_usv-usv8 area33 area53 (1)
anon-sample_water-usv6 usv6store area38 (1)
anon-navigate_usv-usv6 area38 area36 (1)
anon-navigate_usv-usv6 area36 area34 (1)
anon-navigate_usv-usv6 area34 area22 (1)
anon-navigate_usv-usv6 area22 area24 (1)
anon-navigate_usv-usv6 area24 area29 (1)
anon-navigate_usv-usv6 area29 area33 (1)
anon-navigate_usv-usv6 area33 area53 (1)
anon-drop_sample-usv6 usv6store area53 area38 cdm6 (1)
anon-navigate_usv-usv8 area53 area33 (1)
anon-navigate_usv-usv8 area33 area29 (1)
anon-navigate_usv-usv8 area29 area24 (1)
anon-navigate_usv-usv8 area24 area22 (1)
anon-navigate_usv-usv8 area22 area34 (1)
anon-navigate_usv-usv8 area34 area36 (1)
anon-sample_water-usv8 usv8store area36 (1)
anon-navigate_usv-usv8 area36 area34 (1)
anon-navigate_usv-usv8 area34 area22 (1)
anon-navigate_usv-usv8 area22 area24 (1)
anon-navigate_usv-usv8 area24 area29 (1)
anon-navigate_usv-usv8 area29 area33 (1)
anon-navigate_usv-usv8 area33 area53 (1)
anon-navigate_usv-usv8 area53 area13 (1)
anon-drop_sample-usv8 usv8store area13 area36 cdm2 (1)
anon-navigate_usv-usv6 area53 area33 (1)
anon-navigate_usv-usv6 area33 area29 (1)
anon-navigate_usv-usv6 area29 area28 (1)
anon-sample_water-usv6 usv6store area28 (1)
anon-navigate_usv-usv6 area28 area29 (1)
anon-navigate_usv-usv6 area29 area33 (1)
anon-navigate_usv-usv6 area33 area53 (1)
anon-navigate_usv-usv6 area53 area49 (1)
anon-navigate_usv-usv6 area49 area44 (1)
anon-navigate_usv-usv6 area44 area42 (1)
anon-navigate_usv-usv6 area42 area41 (1)
anon-drop_sample-usv6 usv6store area41 area28 cdm5 (1)
anon-navigate_ugv-ugv7 area3 area4 (1)
anon-navigate_ugv-ugv7 area4 area10 (1)
anon-navigate_ugv-ugv7 area10 area13 (1)
anon-navigate_ugv-ugv7 area13 area53 (1)
anon-navigate_ugv-ugv7 area53 area50 (1)
anon-navigate_ugv-ugv7 area50 area47 (1)
anon-navigate_ugv-ugv11 area50 area53 (1)
anon-navigate_ugv-ugv11 area53 area13 (1)
anon-navigate_ugv-ugv11 area13 area10 (1)
anon-navigate_ugv-ugv11 area10 area4 (1)
anon-navigate_ugv-ugv11 area4 area3 (1)
anon-navigate_ugv-ugv11 area3 area1 (1)
anon-pickup_box-ugv11 ugv11store cdm1 area1 box3 (1)
anon-navigate_ugv-ugv11 area1 area3 (1)
anon-navigate_ugv-ugv11 area3 area4 (1)
anon-navigate_ugv-ugv11 area4 area10 (1)
anon-navigate_ugv-ugv11 area10 area13 (1)
anon-navigate_ugv-ugv11 area13 area53 (1)
anon-navigate_ugv-ugv11 area53 area50 (1)
anon-navigate_ugv-ugv11 area50 area47 (1)
anon-navigate_ugv-ugv11 area47 area48 (1)
anon-drop_box-ugv11 ugv11store area48 box3 (1)
anon-navigate_usv-usv8 area13 area53 (1)
anon-navigate_usv-usv8 area53 area49 (1)
anon-navigate_usv-usv8 area49 area44 (1)
anon-navigate_usv-usv8 area44 area42 (1)
anon-navigate_usv-usv8 area42 area41 (1)
anon-sample_water-usv4 usv4store area1 (1)
anon-navigate_usv-usv4 area1 area2 (1)
anon-navigate_usv-usv4 area2 area4 (1)
anon-navigate_usv-usv4 area4 area9 (1)
anon-navigate_usv-usv4 area9 area13 (1)
anon-navigate_usv-usv4 area13 area53 (1)
anon-navigate_usv-usv4 area53 area49 (1)
anon-navigate_usv-usv4 area49 area44 (1)
anon-navigate_usv-usv4 area44 area42 (1)
anon-navigate_usv-usv4 area42 area41 (1)
anon-drop_sample-usv4 usv4store area41 area1 cdm5 (1)
anon-navigate_ugv-ugv7 area47 area50 (1)
anon-navigate_ugv-ugv7 area50 area44 (1)
anon-navigate_ugv-ugv7 area44 area43 (1)
anon-navigate_ugv-ugv7 area43 area41 (1)
anon-pickup_box-ugv7 ugv7store cdm5 area41 box7 (1)
anon-navigate_ugv-ugv7 area41 area43 (1)
anon-navigate_ugv-ugv7 area43 area44 (1)
anon-navigate_ugv-ugv7 area44 area50 (1)
anon-navigate_ugv-ugv7 area50 area53 (1)
anon-navigate_ugv-ugv7 area53 area13 (1)
anon-navigate_ugv-ugv7 area13 area10 (1)
anon-navigate_ugv-ugv7 area10 area7 (1)
anon-navigate_ugv-ugv7 area7 area8 (1)
anon-drop_box-ugv7 ugv7store area8 box7 (1)
anon-navigate_ugv-ugv3 area55 area43 (1)
anon-navigate_ugv-ugv3 area43 area44 (1)
anon-navigate_ugv-ugv3 area44 area50 (1)
anon-navigate_ugv-ugv3 area50 area53 (1)
anon-navigate_ugv-ugv3 area53 area13 (1)
anon-navigate_ugv-ugv3 area13 area10 (1)
anon-navigate_ugv-ugv3 area10 area4 (1)
anon-navigate_ugv-ugv3 area4 area3 (1)
anon-navigate_ugv-ugv3 area3 area1 (1)
anon-pickup_box-ugv3 ugv3store cdm1 area1 box1 (1)
anon-navigate_ugv-ugv3 area1 area3 (1)
anon-navigate_ugv-ugv3 area3 area4 (1)
anon-navigate_ugv-ugv3 area4 area10 (1)
anon-navigate_ugv-ugv3 area10 area13 (1)
anon-navigate_ugv-ugv3 area13 area53 (1)
anon-navigate_ugv-ugv3 area53 area50 (1)
anon-navigate_ugv-ugv3 area50 area44 (1)
anon-navigate_ugv-ugv3 area44 area43 (1)
anon-navigate_ugv-ugv3 area43 area41 (1)
anon-drop_box-ugv3 ugv3store area41 box1 (1)
anon-navigate_usv-usv11 area48 area49 (1)
anon-navigate_usv-usv11 area49 area53 (1)
anon-navigate_usv-usv11 area53 area33 (1)
anon-navigate_usv-usv11 area33 area29 (1)
anon-navigate_usv-usv11 area29 area24 (1)
anon-navigate_usv-usv11 area24 area22 (1)
anon-navigate_usv-usv11 area22 area34 (1)
anon-navigate_usv-usv11 area34 area36 (1)
anon-navigate_usv-usv11 area36 area38 (1)
anon-navigate_usv-usv11 area38 area17 (1)
anon-navigate_usv-usv11 area17 area19 (1)
anon-sample_water-usv11 usv11store area19 (1)
anon-navigate_usv-usv11 area19 area17 (1)
anon-navigate_usv-usv11 area17 area38 (1)
anon-navigate_usv-usv11 area38 area36 (1)
anon-navigate_usv-usv11 area36 area34 (1)
anon-navigate_usv-usv11 area34 area22 (1)
anon-navigate_usv-usv11 area22 area24 (1)
anon-navigate_usv-usv11 area24 area29 (1)
anon-navigate_usv-usv11 area29 area33 (1)
anon-drop_sample-usv11 usv11store area33 area19 cdm4 (1)
anon-navigate_usv-usv11 area33 area53 (1)
anon-navigate_usv-usv11 area53 area49 (1)
anon-navigate_usv-usv11 area49 area44 (1)
anon-navigate_usv-usv11 area44 area42 (1)
anon-navigate_usv-usv11 area42 area46 (1)
anon-navigate_usv-usv11 area46 area52 (1)
anon-navigate_usv-usv11 area52 area50 (1)
anon-sample_water-usv11 usv11store area50 (1)
anon-navigate_usv-usv11 area50 area52 (1)
anon-navigate_usv-usv11 area52 area46 (1)
anon-navigate_usv-usv11 area46 area42 (1)
anon-navigate_usv-usv11 area42 area44 (1)
anon-navigate_usv-usv11 area44 area49 (1)
anon-navigate_usv-usv11 area49 area53 (1)
anon-navigate_usv-usv11 area53 area33 (1)
anon-navigate_usv-usv11 area33 area29 (1)
anon-navigate_usv-usv11 area29 area24 (1)
anon-navigate_usv-usv11 area24 area22 (1)
anon-navigate_usv-usv11 area22 area21 (1)
anon-drop_sample-usv11 usv11store area21 area50 cdm3 (1)
anon-navigate_usv-usv11 area21 area22 (1)
anon-navigate_usv-usv11 area22 area24 (1)
anon-navigate_usv-usv11 area24 area29 (1)
anon-navigate_usv-usv11 area29 area33 (1)
anon-navigate_usv-usv11 area33 area53 (1)
anon-navigate_usv-usv11 area53 area13 (1)
anon-navigate_usv-usv11 area13 area9 (1)
anon-navigate_usv-usv11 area9 area4 (1)
anon-navigate_usv-usv11 area4 area2 (1)
anon-navigate_usv-usv11 area2 area6 (1)
anon-navigate_usv-usv11 area6 area12 (1)
anon-sample_water-usv11 usv11store area12 (1)
anon-navigate_usv-usv11 area12 area6 (1)
anon-navigate_usv-usv11 area6 area2 (1)
anon-navigate_usv-usv11 area2 area14 (1)
anon-navigate_usv-usv11 area14 area16 (1)
anon-navigate_usv-usv11 area16 area18 (1)
anon-navigate_usv-usv11 area18 area20 (1)
anon-navigate_usv-usv11 area20 area19 (1)
anon-navigate_usv-usv11 area19 area17 (1)
anon-navigate_usv-usv11 area17 area38 (1)
anon-navigate_usv-usv11 area38 area36 (1)
anon-navigate_usv-usv11 area36 area21 (1)
anon-drop_sample-usv11 usv11store area21 area12 cdm3 (1)
anon-navigate_usv-usv11 area21 area22 (1)
anon-navigate_usv-usv11 area22 area26 (1)
anon-navigate_usv-usv11 area26 area32 (1)
anon-navigate_usv-usv11 area32 area30 (1)
anon-sample_water-usv11 usv11store area30 (1)
anon-navigate_usv-usv11 area30 area32 (1)
anon-navigate_usv-usv11 area32 area26 (1)
anon-navigate_usv-usv11 area26 area22 (1)
anon-navigate_usv-usv11 area22 area24 (1)
anon-navigate_usv-usv11 area24 area29 (1)
anon-navigate_usv-usv11 area29 area33 (1)
anon-navigate_usv-usv11 area33 area53 (1)
anon-navigate_usv-usv11 area53 area13 (1)
anon-navigate_usv-usv11 area13 area9 (1)
anon-navigate_usv-usv11 area9 area4 (1)
anon-navigate_usv-usv11 area4 area2 (1)
anon-navigate_usv-usv11 area2 area1 (1)
anon-drop_sample-usv11 usv11store area1 area30 cdm1 (1)
Plan length: 308 step(s).
Plan cost: 308
Initial state h value: 160.
Expanded 454 state(s).
Reopened 0 state(s).
Evaluated 455 state(s).
Evaluations: 910
Generated 60676 state(s).
Dead ends: 0 state(s).
Search time: 0.2s
Total time: 0.5s
Solution found.
Peak memory: 25440 KB
