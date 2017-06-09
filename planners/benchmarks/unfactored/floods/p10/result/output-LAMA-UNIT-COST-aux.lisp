INFO     Running translator.
INFO     translator inputs: ['/home/rovane/planners/benchmarks/unfactored/floods/p10/merged-obfuscated-domain.pddl', '/home/rovane/planners/benchmarks/unfactored/floods/p10/merged-obfuscated-problem.pddl']
INFO     translator arguments: []
Parsing...
Parsing: [0.010s CPU, 0.014s wall-clock]
Normalizing task... [0.000s CPU, 0.001s wall-clock]
Instantiating...
Generating Datalog program... [0.010s CPU, 0.005s wall-clock]
Normalizing Datalog program...
Normalizing Datalog program: [0.010s CPU, 0.016s wall-clock]
Preparing model... [0.010s CPU, 0.013s wall-clock]
Generated 359 rules.
Computing model... [0.430s CPU, 0.427s wall-clock]
22790 relevant atoms
6125 auxiliary atoms
28915 final queue length
51427 total queue pushes
Completing instantiation... [0.910s CPU, 0.905s wall-clock]
Instantiating: [1.370s CPU, 1.376s wall-clock]
Computing fact groups...
Finding invariants...
87 initial candidates
Finding invariants: [0.070s CPU, 0.066s wall-clock]
Checking invariant weight... [0.000s CPU, 0.001s wall-clock]
Instantiating groups... [0.020s CPU, 0.021s wall-clock]
Collecting mutex groups... [0.010s CPU, 0.002s wall-clock]
Choosing groups...
847 uncovered facts
Choosing groups: [0.000s CPU, 0.004s wall-clock]
Building translation key... [0.010s CPU, 0.006s wall-clock]
Computing fact groups: [0.120s CPU, 0.118s wall-clock]
Building STRIPS to SAS dictionary... [0.000s CPU, 0.003s wall-clock]
Building dictionary for full mutex groups... [0.000s CPU, 0.002s wall-clock]
Building mutex information...
Building mutex information: [0.000s CPU, 0.003s wall-clock]
Translating task...
Processing axioms...
Simplifying axioms... [0.000s CPU, 0.000s wall-clock]
Processing axioms: [0.040s CPU, 0.033s wall-clock]
Translating task: [0.860s CPU, 0.857s wall-clock]
9449 effect conditions simplified
0 implied preconditions added
Detecting unreachable propositions...
0 operators removed
51 propositions removed
Detecting unreachable propositions: [0.140s CPU, 0.136s wall-clock]
Translator variables: 856
Translator derived variables: 0
Translator facts: 2986
Translator goal facts: 36
Translator mutex groups: 73
Translator total mutex groups size: 1420
Translator operators: 19812
Translator axioms: 0
Translator task size: 86702
Translator peak memory: 112296 KB
Writing output... [0.160s CPU, 0.158s wall-clock]
Done! [2.690s CPU, 2.706s wall-clock]
INFO     Running preprocessor.
INFO     preprocessor input: output.sas
INFO     preprocessor arguments: []
Building causal graph...
The causal graph is not acyclic.
532 variables of 856 necessary
0 of 73 mutex groups necessary.
19812 of 19812 operators necessary.
0 of 0 axiom rules necessary.
Building domain transition graphs...
solveable in poly time 0
Building successor generator...
Preprocessor facts: 2338
Preprocessor derived variables: 0
Preprocessor task size: 78100
Writing output...
done
INFO     Running search.
INFO     search input: output
INFO     search executable: /home/rovane/planners/cmap/planning/fd/src/search/downward-release
INFO     search arguments: ['--heuristic', 'hlm,hff=lm_ff_syn(lm_rhw(reasonable_orders=true,lm_cost_type=1,cost_type=1))', '--search', 'lazy_greedy(hff,hlm,preferred=[hff,hlm])', '--internal-plan-file', '/home/rovane/planners/benchmarks/unfactored/floods/p10/result/plan-LAMA-UNIT-COST.lisp']
reading input... [t=0s]
Simplifying transitions... done!
done reading input! [t=0.29s]
building causal graph...done! [t=0.29s]
packing state variables...done! [t=0.29s]
Variables: 532
Facts: 2338
Bytes per state: 84
done initalizing global data [t=0.29s]
Initializing Exploration...
Generating landmarks using the RPG/SAS+ approach
approx. reasonable orders
approx. obedient reasonable orders
Removed 0 reasonable or obedient reasonable orders
Landmarks generation time: 0.166439s
Discovered 72 landmarks, of which 0 are disjunctive and 0 are conjunctive 
36 edges
Initializing LAMA-FF Synergy Object
Initializing landmarks count heuristic...
36 initial landmarks, 36 goal landmarks
Conducting lazy best first search, (real) bound = 2147483647
Best heuristic value: 176 [g=0, 1 evaluated, 0 expanded, t=0.46s, 34312 KB]
Best heuristic value: 175 [g=1, 2 evaluated, 1 expanded, t=0.47s, 34312 KB]
Best heuristic value: 174 [g=2, 3 evaluated, 2 expanded, t=0.47s, 34312 KB]
Best heuristic value: 173 [g=3, 4 evaluated, 3 expanded, t=0.47s, 34312 KB]
Best heuristic value: 172 [g=4, 5 evaluated, 4 expanded, t=0.47s, 34312 KB]
Best heuristic value: 171 [g=6, 8 evaluated, 7 expanded, t=0.47s, 34312 KB]
Best heuristic value: 170 [g=7, 9 evaluated, 8 expanded, t=0.47s, 34312 KB]
Best heuristic value: 169 [g=8, 10 evaluated, 9 expanded, t=0.47s, 34312 KB]
Best heuristic value: 168 [g=10, 12 evaluated, 11 expanded, t=0.47s, 34312 KB]
Best heuristic value: 167 [g=11, 13 evaluated, 12 expanded, t=0.47s, 34312 KB]
Best heuristic value: 166 [g=12, 14 evaluated, 13 expanded, t=0.47s, 34312 KB]
Best heuristic value: 165 [g=14, 16 evaluated, 15 expanded, t=0.47s, 34312 KB]
Best heuristic value: 164 [g=15, 17 evaluated, 16 expanded, t=0.47s, 34312 KB]
Best heuristic value: 163 [g=16, 18 evaluated, 17 expanded, t=0.47s, 34312 KB]
Best heuristic value: 162 [g=18, 20 evaluated, 19 expanded, t=0.47s, 34312 KB]
Best heuristic value: 161 [g=19, 21 evaluated, 20 expanded, t=0.47s, 34312 KB]
Best heuristic value: 160 [g=20, 22 evaluated, 21 expanded, t=0.47s, 34312 KB]
Best heuristic value: 159 [g=21, 23 evaluated, 22 expanded, t=0.48s, 34312 KB]
Best heuristic value: 158 [g=22, 24 evaluated, 23 expanded, t=0.48s, 34444 KB]
Best heuristic value: 157 [g=23, 25 evaluated, 24 expanded, t=0.48s, 34444 KB]
Best heuristic value: 156 [g=25, 28 evaluated, 27 expanded, t=0.48s, 34444 KB]
Best heuristic value: 155 [g=26, 29 evaluated, 28 expanded, t=0.48s, 34444 KB]
Best heuristic value: 154 [g=27, 30 evaluated, 29 expanded, t=0.48s, 34444 KB]
Best heuristic value: 153 [g=29, 32 evaluated, 31 expanded, t=0.48s, 34444 KB]
Best heuristic value: 152 [g=30, 33 evaluated, 32 expanded, t=0.49s, 34444 KB]
Best heuristic value: 151 [g=31, 34 evaluated, 33 expanded, t=0.49s, 34444 KB]
Best heuristic value: 150 [g=33, 36 evaluated, 35 expanded, t=0.49s, 34444 KB]
Best heuristic value: 149 [g=34, 37 evaluated, 36 expanded, t=0.49s, 34444 KB]
Best heuristic value: 148 [g=35, 38 evaluated, 37 expanded, t=0.49s, 34444 KB]
Best heuristic value: 147 [g=37, 40 evaluated, 39 expanded, t=0.49s, 34444 KB]
Best heuristic value: 146 [g=38, 41 evaluated, 40 expanded, t=0.49s, 34444 KB]
Best heuristic value: 145 [g=39, 42 evaluated, 41 expanded, t=0.49s, 34444 KB]
Best heuristic value: 144 [g=41, 44 evaluated, 43 expanded, t=0.49s, 34444 KB]
Best heuristic value: 143 [g=42, 45 evaluated, 44 expanded, t=0.49s, 34444 KB]
Best heuristic value: 142 [g=43, 46 evaluated, 45 expanded, t=0.49s, 34444 KB]
Best heuristic value: 141 [g=44, 47 evaluated, 46 expanded, t=0.49s, 34444 KB]
Best heuristic value: 140 [g=45, 48 evaluated, 47 expanded, t=0.5s, 34444 KB]
Best heuristic value: 139 [g=46, 49 evaluated, 48 expanded, t=0.5s, 34444 KB]
Best heuristic value: 138 [g=48, 52 evaluated, 51 expanded, t=0.5s, 34444 KB]
Best heuristic value: 137 [g=49, 53 evaluated, 52 expanded, t=0.5s, 34444 KB]
Best heuristic value: 136 [g=50, 54 evaluated, 53 expanded, t=0.5s, 34444 KB]
Best heuristic value: 135 [g=52, 56 evaluated, 55 expanded, t=0.5s, 34444 KB]
Best heuristic value: 134 [g=53, 57 evaluated, 56 expanded, t=0.5s, 34576 KB]
Best heuristic value: 133 [g=54, 58 evaluated, 57 expanded, t=0.51s, 34576 KB]
Best heuristic value: 132 [g=56, 60 evaluated, 59 expanded, t=0.51s, 34576 KB]
Best heuristic value: 131 [g=57, 61 evaluated, 60 expanded, t=0.51s, 34576 KB]
Best heuristic value: 130 [g=58, 62 evaluated, 61 expanded, t=0.51s, 34576 KB]
Best heuristic value: 128 [g=60, 65 evaluated, 64 expanded, t=0.51s, 34576 KB]
Best heuristic value: 127 [g=68, 76 evaluated, 75 expanded, t=0.52s, 34576 KB]
Best heuristic value: 126 [g=69, 77 evaluated, 76 expanded, t=0.52s, 34576 KB]
Best heuristic value: 125 [g=73, 81 evaluated, 80 expanded, t=0.52s, 34576 KB]
Best heuristic value: 124 [g=75, 83 evaluated, 82 expanded, t=0.52s, 34576 KB]
Best heuristic value: 123 [g=76, 84 evaluated, 83 expanded, t=0.52s, 34576 KB]
Best heuristic value: 122 [g=77, 85 evaluated, 84 expanded, t=0.53s, 34576 KB]
Best heuristic value: 121 [g=85, 93 evaluated, 92 expanded, t=0.53s, 34708 KB]
Best heuristic value: 120 [g=86, 94 evaluated, 93 expanded, t=0.53s, 34708 KB]
Best heuristic value: 119 [g=87, 95 evaluated, 94 expanded, t=0.53s, 34708 KB]
Best heuristic value: 118 [g=89, 100 evaluated, 99 expanded, t=0.53s, 34708 KB]
Best heuristic value: 117 [g=90, 101 evaluated, 100 expanded, t=0.54s, 34708 KB]
Best heuristic value: 116 [g=91, 102 evaluated, 101 expanded, t=0.54s, 34708 KB]
Best heuristic value: 115 [g=92, 103 evaluated, 102 expanded, t=0.54s, 34708 KB]
Best heuristic value: 114 [g=93, 104 evaluated, 103 expanded, t=0.54s, 34708 KB]
Best heuristic value: 113 [g=94, 105 evaluated, 104 expanded, t=0.54s, 34708 KB]
Best heuristic value: 112 [g=96, 107 evaluated, 106 expanded, t=0.54s, 34708 KB]
Best heuristic value: 111 [g=105, 123 evaluated, 122 expanded, t=0.55s, 34840 KB]
Best heuristic value: 110 [g=106, 124 evaluated, 123 expanded, t=0.55s, 34840 KB]
Best heuristic value: 109 [g=107, 126 evaluated, 125 expanded, t=0.55s, 34840 KB]
Best heuristic value: 108 [g=108, 127 evaluated, 126 expanded, t=0.55s, 34840 KB]
Best heuristic value: 107 [g=109, 128 evaluated, 127 expanded, t=0.56s, 34840 KB]
Best heuristic value: 106 [g=110, 129 evaluated, 128 expanded, t=0.56s, 34840 KB]
Best heuristic value: 105 [g=111, 130 evaluated, 129 expanded, t=0.56s, 34840 KB]
Best heuristic value: 104 [g=112, 131 evaluated, 130 expanded, t=0.56s, 34840 KB]
Best heuristic value: 103 [g=113, 132 evaluated, 131 expanded, t=0.56s, 34840 KB]
Best heuristic value: 100 [g=115, 134 evaluated, 133 expanded, t=0.56s, 34840 KB]
Best heuristic value: 97 [g=131, 152 evaluated, 151 expanded, t=0.57s, 34840 KB]
Best heuristic value: 96 [g=140, 169 evaluated, 168 expanded, t=0.59s, 34972 KB]
Best heuristic value: 95 [g=141, 170 evaluated, 169 expanded, t=0.59s, 34972 KB]
Best heuristic value: 94 [g=142, 171 evaluated, 170 expanded, t=0.59s, 34972 KB]
Best heuristic value: 93 [g=143, 172 evaluated, 171 expanded, t=0.59s, 34972 KB]
Best heuristic value: 92 [g=144, 173 evaluated, 172 expanded, t=0.59s, 34972 KB]
Best heuristic value: 91 [g=145, 174 evaluated, 173 expanded, t=0.59s, 34972 KB]
Best heuristic value: 90 [g=146, 175 evaluated, 174 expanded, t=0.59s, 34972 KB]
Best heuristic value: 89 [g=147, 176 evaluated, 175 expanded, t=0.59s, 34972 KB]
Best heuristic value: 88 [g=148, 177 evaluated, 176 expanded, t=0.6s, 34972 KB]
Best heuristic value: 87 [g=151, 181 evaluated, 180 expanded, t=0.6s, 34972 KB]
Best heuristic value: 82 [g=152, 182 evaluated, 181 expanded, t=0.6s, 34972 KB]
Best heuristic value: 80 [g=164, 194 evaluated, 193 expanded, t=0.61s, 35104 KB]
Best heuristic value: 75 [g=177, 208 evaluated, 207 expanded, t=0.62s, 35104 KB]
Best heuristic value: 74 [g=195, 231 evaluated, 230 expanded, t=0.64s, 35236 KB]
Best heuristic value: 73 [g=196, 232 evaluated, 231 expanded, t=0.64s, 35236 KB]
Best heuristic value: 72 [g=197, 233 evaluated, 232 expanded, t=0.64s, 35236 KB]
Best heuristic value: 71 [g=198, 234 evaluated, 233 expanded, t=0.64s, 35236 KB]
Best heuristic value: 70 [g=199, 236 evaluated, 235 expanded, t=0.64s, 35236 KB]
Best heuristic value: 69 [g=200, 237 evaluated, 236 expanded, t=0.64s, 35236 KB]
Best heuristic value: 68 [g=201, 238 evaluated, 237 expanded, t=0.64s, 35236 KB]
Best heuristic value: 67 [g=202, 239 evaluated, 238 expanded, t=0.64s, 35236 KB]
Best heuristic value: 66 [g=203, 240 evaluated, 239 expanded, t=0.64s, 35236 KB]
Best heuristic value: 65 [g=204, 241 evaluated, 240 expanded, t=0.64s, 35236 KB]
Best heuristic value: 64 [g=205, 242 evaluated, 241 expanded, t=0.64s, 35236 KB]
Best heuristic value: 63 [g=206, 243 evaluated, 242 expanded, t=0.64s, 35236 KB]
Best heuristic value: 62 [g=207, 244 evaluated, 243 expanded, t=0.64s, 35236 KB]
Best heuristic value: 61 [g=208, 245 evaluated, 244 expanded, t=0.64s, 35236 KB]
Best heuristic value: 60 [g=218, 255 evaluated, 254 expanded, t=0.65s, 35368 KB]
Best heuristic value: 59 [g=219, 256 evaluated, 255 expanded, t=0.65s, 35368 KB]
Best heuristic value: 58 [g=220, 257 evaluated, 256 expanded, t=0.65s, 35368 KB]
Best heuristic value: 57 [g=221, 258 evaluated, 257 expanded, t=0.65s, 35368 KB]
Best heuristic value: 56 [g=222, 259 evaluated, 258 expanded, t=0.65s, 35368 KB]
Best heuristic value: 55 [g=223, 260 evaluated, 259 expanded, t=0.65s, 35368 KB]
Best heuristic value: 54 [g=224, 261 evaluated, 260 expanded, t=0.65s, 35368 KB]
Best heuristic value: 53 [g=225, 262 evaluated, 261 expanded, t=0.65s, 35368 KB]
Best heuristic value: 52 [g=226, 263 evaluated, 262 expanded, t=0.65s, 35368 KB]
Best heuristic value: 51 [g=227, 264 evaluated, 263 expanded, t=0.65s, 35368 KB]
Best heuristic value: 50 [g=237, 281 evaluated, 280 expanded, t=0.66s, 35500 KB]
Best heuristic value: 49 [g=238, 282 evaluated, 281 expanded, t=0.66s, 35500 KB]
Best heuristic value: 48 [g=239, 283 evaluated, 282 expanded, t=0.66s, 35500 KB]
Best heuristic value: 47 [g=240, 284 evaluated, 283 expanded, t=0.66s, 35500 KB]
Best heuristic value: 46 [g=241, 285 evaluated, 284 expanded, t=0.66s, 35500 KB]
Best heuristic value: 45 [g=242, 286 evaluated, 285 expanded, t=0.67s, 35500 KB]
Best heuristic value: 44 [g=247, 306 evaluated, 305 expanded, t=0.68s, 35500 KB]
Best heuristic value: 43 [g=248, 307 evaluated, 306 expanded, t=0.68s, 35500 KB]
Best heuristic value: 42 [g=249, 308 evaluated, 307 expanded, t=0.68s, 35500 KB]
Best heuristic value: 41 [g=250, 309 evaluated, 308 expanded, t=0.68s, 35500 KB]
Best heuristic value: 40 [g=251, 310 evaluated, 309 expanded, t=0.68s, 35500 KB]
Best heuristic value: 39 [g=252, 311 evaluated, 310 expanded, t=0.68s, 35500 KB]
Best heuristic value: 38 [g=253, 312 evaluated, 311 expanded, t=0.68s, 35632 KB]
Best heuristic value: 37 [g=254, 313 evaluated, 312 expanded, t=0.68s, 35632 KB]
Best heuristic value: 36 [g=255, 314 evaluated, 313 expanded, t=0.68s, 35632 KB]
Best heuristic value: 35 [g=256, 315 evaluated, 314 expanded, t=0.68s, 35632 KB]
Best heuristic value: 34 [g=257, 316 evaluated, 315 expanded, t=0.69s, 35632 KB]
Best heuristic value: 28 [g=260, 321 evaluated, 320 expanded, t=0.69s, 35632 KB]
Best heuristic value: 27 [g=271, 334 evaluated, 333 expanded, t=0.7s, 35632 KB]
Best heuristic value: 25 [g=272, 335 evaluated, 334 expanded, t=0.7s, 35632 KB]
Best heuristic value: 18 [g=273, 336 evaluated, 335 expanded, t=0.7s, 35632 KB]
Best heuristic value: 17 [g=274, 337 evaluated, 336 expanded, t=0.7s, 35632 KB]
Best heuristic value: 16 [g=288, 353 evaluated, 352 expanded, t=0.71s, 35764 KB]
Best heuristic value: 15 [g=289, 354 evaluated, 353 expanded, t=0.71s, 35764 KB]
Best heuristic value: 14 [g=290, 355 evaluated, 354 expanded, t=0.71s, 35764 KB]
Best heuristic value: 13 [g=291, 356 evaluated, 355 expanded, t=0.71s, 35764 KB]
Best heuristic value: 12 [g=292, 357 evaluated, 356 expanded, t=0.71s, 35764 KB]
Best heuristic value: 11 [g=302, 368 evaluated, 367 expanded, t=0.72s, 35764 KB]
Best heuristic value: 10 [g=303, 369 evaluated, 368 expanded, t=0.72s, 35764 KB]
Best heuristic value: 9 [g=304, 370 evaluated, 369 expanded, t=0.72s, 35764 KB]
Best heuristic value: 8 [g=305, 371 evaluated, 370 expanded, t=0.72s, 35764 KB]
Best heuristic value: 7 [g=306, 372 evaluated, 371 expanded, t=0.72s, 35764 KB]
Best heuristic value: 6 [g=307, 373 evaluated, 372 expanded, t=0.72s, 35764 KB]
Best heuristic value: 5 [g=308, 374 evaluated, 373 expanded, t=0.72s, 35764 KB]
Best heuristic value: 4 [g=309, 375 evaluated, 374 expanded, t=0.72s, 35764 KB]
Best heuristic value: 3 [g=310, 376 evaluated, 375 expanded, t=0.72s, 35764 KB]
Best heuristic value: 2 [g=311, 377 evaluated, 376 expanded, t=0.72s, 35764 KB]
Best heuristic value: 1 [g=312, 378 evaluated, 377 expanded, t=0.72s, 35764 KB]
Solution found!
Actual search time: 0.26s [t=0.72s]
anon-take_picture-usv9 area41 disaster9 (1)
anon-communicate_data-usv9 cdm5 disaster9 area41 area41 (1)
anon-take_picture-usv3 area13 disaster5 (1)
anon-communicate_data-usv3 cdm2 disaster5 area13 area13 (1)
anon-navigate_uav-uav11 area53 area11 (1)
anon-take_picture-uav11 area11 disaster8 (1)
anon-navigate_uav-uav11 area11 area9 (1)
anon-communicate_data-uav11 cdm2 disaster8 area9 area13 (1)
anon-navigate_uav-uav11 area9 area55 (1)
anon-take_picture-uav11 area55 disaster7 (1)
anon-navigate_uav-uav11 area55 area9 (1)
anon-communicate_data-uav11 cdm2 disaster7 area9 area13 (1)
anon-navigate_uav-uav11 area9 area7 (1)
anon-take_picture-uav11 area7 disaster6 (1)
anon-navigate_uav-uav11 area7 area9 (1)
anon-communicate_data-uav11 cdm2 disaster6 area9 area13 (1)
anon-navigate_uav-uav11 area9 area36 (1)
anon-take_picture-uav11 area36 disaster4 (1)
anon-navigate_uav-uav11 area36 area9 (1)
anon-communicate_data-uav11 cdm2 disaster4 area9 area13 (1)
anon-navigate_uav-uav11 area9 area49 (1)
anon-take_picture-uav11 area49 disaster3 (1)
anon-communicate_data-uav11 cdm6 disaster3 area49 area53 (1)
anon-navigate_uav-uav11 area49 area27 (1)
anon-take_picture-uav11 area27 disaster2 (1)
anon-navigate_uav-uav11 area27 area9 (1)
anon-communicate_data-uav11 cdm2 disaster2 area9 area13 (1)
anon-navigate_uav-uav11 area9 area28 (1)
anon-take_picture-uav11 area28 disaster16 (1)
anon-navigate_uav-uav11 area28 area9 (1)
anon-communicate_data-uav11 cdm2 disaster16 area9 area13 (1)
anon-navigate_uav-uav11 area9 area20 (1)
anon-take_picture-uav11 area20 disaster15 (1)
anon-navigate_uav-uav11 area20 area9 (1)
anon-communicate_data-uav11 cdm2 disaster15 area9 area13 (1)
anon-navigate_uav-uav11 area9 area38 (1)
anon-take_picture-uav11 area38 disaster14 (1)
anon-navigate_uav-uav11 area38 area9 (1)
anon-communicate_data-uav11 cdm2 disaster14 area9 area13 (1)
anon-navigate_uav-uav11 area9 area44 (1)
anon-take_picture-uav11 area44 disaster13 (1)
anon-navigate_uav-uav11 area44 area9 (1)
anon-communicate_data-uav11 cdm2 disaster13 area9 area13 (1)
anon-navigate_uav-uav11 area9 area30 (1)
anon-take_picture-uav11 area30 disaster12 (1)
anon-communicate_data-uav11 cdm4 disaster12 area30 area33 (1)
anon-navigate_uav-uav11 area30 area24 (1)
anon-take_picture-uav11 area24 disaster11 (1)
anon-navigate_uav-uav11 area24 area9 (1)
anon-communicate_data-uav11 cdm2 disaster11 area9 area13 (1)
anon-navigate_uav-uav11 area9 area37 (1)
anon-take_picture-uav11 area37 disaster10 (1)
anon-navigate_uav-uav11 area37 area9 (1)
anon-communicate_data-uav11 cdm2 disaster10 area9 area13 (1)
anon-navigate_uav-uav11 area9 area58 (1)
anon-take_picture-uav11 area58 disaster1 (1)
anon-navigate_uav-uav11 area58 area9 (1)
anon-communicate_data-uav11 cdm2 disaster1 area9 area13 (1)
anon-navigate_ugv-ugv11 area53 area33 (1)
anon-pickup_box-ugv3 ugv3store cdm2 area13 box2 (1)
anon-navigate_ugv-ugv3 area13 area53 (1)
anon-navigate_ugv-ugv3 area53 area33 (1)
anon-drop_box-ugv3 ugv3store area33 box2 (1)
anon-navigate_usv-usv3 area13 area9 (1)
anon-navigate_usv-usv3 area9 area4 (1)
anon-sample_water-usv3 usv3store area4 (1)
anon-navigate_usv-usv3 area4 area9 (1)
anon-navigate_usv-usv3 area9 area13 (1)
anon-navigate_usv-usv3 area13 area53 (1)
anon-drop_sample-usv3 usv3store area53 area4 cdm6 (1)
anon-navigate_usv-usv5 area21 area36 (1)
anon-navigate_usv-usv5 area36 area38 (1)
anon-navigate_usv-usv5 area38 area17 (1)
anon-sample_water-usv5 usv5store area17 (1)
anon-navigate_usv-usv5 area17 area38 (1)
anon-navigate_usv-usv5 area38 area36 (1)
anon-navigate_usv-usv5 area36 area21 (1)
anon-drop_sample-usv5 usv5store area21 area17 cdm3 (1)
anon-navigate_usv-usv11 area53 area13 (1)
anon-navigate_usv-usv11 area13 area9 (1)
anon-navigate_usv-usv11 area9 area8 (1)
anon-sample_water-usv11 usv11store area8 (1)
anon-navigate_usv-usv11 area8 area9 (1)
anon-navigate_usv-usv11 area9 area13 (1)
anon-navigate_usv-usv11 area13 area53 (1)
anon-navigate_usv-usv11 area53 area33 (1)
anon-drop_sample-usv11 usv11store area33 area8 cdm4 (1)
anon-navigate_usv-usv9 area41 area56 (1)
anon-sample_water-usv9 usv9store area56 (1)
anon-navigate_usv-usv9 area56 area54 (1)
anon-navigate_usv-usv9 area54 area42 (1)
anon-navigate_usv-usv9 area42 area44 (1)
anon-navigate_usv-usv9 area44 area49 (1)
anon-navigate_usv-usv9 area49 area53 (1)
anon-drop_sample-usv9 usv9store area53 area56 cdm6 (1)
anon-navigate_usv-usv3 area53 area49 (1)
anon-navigate_usv-usv3 area49 area44 (1)
anon-navigate_usv-usv3 area44 area42 (1)
anon-navigate_usv-usv9 area53 area33 (1)
anon-sample_water-usv9 usv9store area33 (1)
anon-navigate_usv-usv9 area33 area53 (1)
anon-navigate_usv-usv9 area53 area49 (1)
anon-navigate_usv-usv9 area49 area44 (1)
anon-navigate_usv-usv9 area44 area42 (1)
anon-navigate_usv-usv9 area42 area41 (1)
anon-drop_sample-usv9 usv9store area41 area33 cdm5 (1)
anon-pickup_box-ugv7 ugv7store cdm4 area33 box6 (1)
anon-navigate_ugv-ugv7 area33 area30 (1)
anon-navigate_ugv-ugv7 area30 area24 (1)
anon-navigate_ugv-ugv7 area24 area23 (1)
anon-navigate_ugv-ugv7 area23 area25 (1)
anon-navigate_ugv-ugv7 area25 area31 (1)
anon-drop_box-ugv7 ugv7store area31 box6 (1)
anon-navigate_usv-usv1 area1 area16 (1)
anon-navigate_usv-usv1 area16 area18 (1)
anon-navigate_usv-usv1 area18 area20 (1)
anon-navigate_usv-usv1 area20 area19 (1)
anon-navigate_usv-usv1 area19 area17 (1)
anon-navigate_usv-usv1 area17 area38 (1)
anon-navigate_usv-usv1 area38 area40 (1)
anon-sample_water-usv1 usv1store area40 (1)
anon-navigate_usv-usv1 area40 area38 (1)
anon-navigate_usv-usv1 area38 area17 (1)
anon-navigate_usv-usv1 area17 area19 (1)
anon-navigate_usv-usv1 area19 area20 (1)
anon-navigate_usv-usv1 area20 area18 (1)
anon-navigate_usv-usv1 area18 area16 (1)
anon-navigate_usv-usv1 area16 area1 (1)
anon-drop_sample-usv1 usv1store area1 area40 cdm1 (1)
anon-navigate_usv-usv11 area33 area53 (1)
anon-navigate_usv-usv11 area53 area49 (1)
anon-navigate_usv-usv11 area49 area44 (1)
anon-navigate_usv-usv11 area44 area42 (1)
anon-navigate_usv-usv3 area42 area44 (1)
anon-navigate_usv-usv3 area44 area49 (1)
anon-navigate_usv-usv3 area49 area53 (1)
anon-navigate_usv-usv3 area53 area33 (1)
anon-navigate_usv-usv3 area33 area29 (1)
anon-navigate_usv-usv3 area29 area28 (1)
anon-sample_water-usv3 usv3store area28 (1)
anon-navigate_usv-usv3 area28 area29 (1)
anon-navigate_usv-usv3 area29 area33 (1)
anon-navigate_usv-usv3 area33 area53 (1)
anon-navigate_usv-usv3 area53 area49 (1)
anon-navigate_usv-usv3 area49 area44 (1)
anon-navigate_usv-usv3 area44 area42 (1)
anon-navigate_usv-usv3 area42 area41 (1)
anon-drop_sample-usv3 usv3store area41 area28 cdm5 (1)
anon-navigate_ugv-ugv5 area21 area36 (1)
anon-navigate_ugv-ugv5 area36 area37 (1)
anon-navigate_ugv-ugv5 area37 area18 (1)
anon-navigate_ugv-ugv5 area18 area15 (1)
anon-navigate_ugv-ugv5 area15 area3 (1)
anon-navigate_ugv-ugv5 area3 area4 (1)
anon-navigate_ugv-ugv5 area4 area10 (1)
anon-navigate_ugv-ugv5 area10 area13 (1)
anon-navigate_ugv-ugv5 area13 area53 (1)
anon-pickup_box-ugv5 ugv5store cdm6 area53 box9 (1)
anon-navigate_ugv-ugv5 area53 area13 (1)
anon-navigate_ugv-ugv5 area13 area10 (1)
anon-navigate_ugv-ugv5 area10 area4 (1)
anon-navigate_ugv-ugv5 area4 area3 (1)
anon-navigate_ugv-ugv5 area3 area15 (1)
anon-drop_box-ugv5 ugv5store area15 box9 (1)
anon-navigate_ugv-ugv5 area15 area3 (1)
anon-navigate_ugv-ugv5 area3 area4 (1)
anon-navigate_ugv-ugv5 area4 area10 (1)
anon-navigate_ugv-ugv5 area10 area13 (1)
anon-navigate_ugv-ugv5 area13 area53 (1)
anon-pickup_box-ugv5 ugv5store cdm6 area53 box10 (1)
anon-navigate_ugv-ugv5 area53 area13 (1)
anon-navigate_ugv-ugv5 area13 area10 (1)
anon-navigate_ugv-ugv5 area10 area4 (1)
anon-navigate_ugv-ugv5 area4 area3 (1)
anon-navigate_ugv-ugv5 area3 area15 (1)
anon-navigate_ugv-ugv5 area15 area18 (1)
anon-drop_box-ugv5 ugv5store area18 box10 (1)
anon-navigate_ugv-ugv5 area18 area37 (1)
anon-navigate_ugv-ugv5 area37 area36 (1)
anon-navigate_ugv-ugv5 area36 area21 (1)
anon-pickup_box-ugv5 ugv5store cdm3 area21 box5 (1)
anon-navigate_ugv-ugv5 area21 area36 (1)
anon-navigate_ugv-ugv5 area36 area37 (1)
anon-navigate_ugv-ugv5 area37 area18 (1)
anon-navigate_ugv-ugv5 area18 area15 (1)
anon-navigate_ugv-ugv5 area15 area3 (1)
anon-navigate_ugv-ugv5 area3 area4 (1)
anon-navigate_ugv-ugv5 area4 area6 (1)
anon-drop_box-ugv5 ugv5store area6 box5 (1)
anon-pickup_box-ugv9 ugv9store cdm5 area41 box7 (1)
anon-navigate_ugv-ugv9 area41 area43 (1)
anon-navigate_ugv-ugv9 area43 area44 (1)
anon-navigate_ugv-ugv9 area44 area50 (1)
anon-navigate_ugv-ugv9 area50 area53 (1)
anon-navigate_ugv-ugv9 area53 area13 (1)
anon-navigate_ugv-ugv9 area13 area10 (1)
anon-navigate_ugv-ugv9 area10 area7 (1)
anon-drop_box-ugv9 ugv9store area7 box7 (1)
anon-pickup_box-ugv11 ugv11store cdm4 area33 box4 (1)
anon-navigate_ugv-ugv11 area33 area53 (1)
anon-navigate_ugv-ugv11 area53 area50 (1)
anon-navigate_ugv-ugv11 area50 area44 (1)
anon-navigate_ugv-ugv11 area44 area43 (1)
anon-navigate_ugv-ugv11 area43 area55 (1)
anon-navigate_ugv-ugv11 area55 area56 (1)
anon-navigate_ugv-ugv11 area56 area57 (1)
anon-navigate_ugv-ugv11 area57 area54 (1)
anon-drop_box-ugv11 ugv11store area54 box4 (1)
anon-navigate_usv-usv1 area1 area16 (1)
anon-navigate_usv-usv1 area16 area18 (1)
anon-navigate_usv-usv1 area18 area20 (1)
anon-navigate_usv-usv1 area20 area19 (1)
anon-navigate_usv-usv1 area19 area17 (1)
anon-navigate_usv-usv1 area17 area38 (1)
anon-navigate_usv-usv1 area38 area40 (1)
anon-navigate_usv-usv1 area40 area39 (1)
anon-navigate_usv-usv1 area39 area37 (1)
anon-sample_water-usv1 usv1store area37 (1)
anon-navigate_usv-usv1 area37 area39 (1)
anon-navigate_usv-usv1 area39 area40 (1)
anon-navigate_usv-usv1 area40 area38 (1)
anon-navigate_usv-usv1 area38 area17 (1)
anon-navigate_usv-usv1 area17 area19 (1)
anon-navigate_usv-usv1 area19 area20 (1)
anon-navigate_usv-usv1 area20 area18 (1)
anon-navigate_usv-usv1 area18 area16 (1)
anon-navigate_usv-usv1 area16 area1 (1)
anon-drop_sample-usv1 usv1store area1 area37 cdm1 (1)
anon-navigate_usv-usv5 area21 area36 (1)
anon-navigate_usv-usv5 area36 area38 (1)
anon-navigate_usv-usv5 area38 area35 (1)
anon-sample_water-usv5 usv5store area35 (1)
anon-navigate_usv-usv5 area35 area38 (1)
anon-navigate_usv-usv5 area38 area36 (1)
anon-navigate_usv-usv5 area36 area34 (1)
anon-navigate_usv-usv5 area34 area22 (1)
anon-navigate_usv-usv5 area22 area24 (1)
anon-navigate_usv-usv5 area24 area29 (1)
anon-navigate_usv-usv5 area29 area33 (1)
anon-navigate_usv-usv5 area33 area53 (1)
anon-navigate_usv-usv5 area53 area13 (1)
anon-drop_sample-usv5 usv5store area13 area35 cdm2 (1)
anon-navigate_usv-usv5 area13 area9 (1)
anon-navigate_usv-usv5 area9 area4 (1)
anon-navigate_usv-usv1 area1 area2 (1)
anon-navigate_usv-usv5 area4 area2 (1)
anon-sample_water-usv5 usv5store area2 (1)
anon-navigate_usv-usv5 area2 area14 (1)
anon-navigate_usv-usv5 area14 area16 (1)
anon-navigate_usv-usv5 area16 area18 (1)
anon-navigate_usv-usv5 area18 area20 (1)
anon-navigate_usv-usv5 area20 area19 (1)
anon-navigate_usv-usv5 area19 area17 (1)
anon-navigate_usv-usv5 area17 area38 (1)
anon-navigate_usv-usv5 area38 area36 (1)
anon-navigate_usv-usv5 area36 area21 (1)
anon-drop_sample-usv5 usv5store area21 area2 cdm3 (1)
anon-navigate_ugv-ugv3 area33 area53 (1)
anon-navigate_ugv-ugv3 area53 area50 (1)
anon-navigate_ugv-ugv3 area50 area44 (1)
anon-navigate_ugv-ugv3 area44 area43 (1)
anon-navigate_ugv-ugv3 area43 area41 (1)
anon-pickup_box-ugv3 ugv3store cdm5 area41 box8 (1)
anon-navigate_ugv-ugv3 area41 area43 (1)
anon-navigate_ugv-ugv3 area43 area44 (1)
anon-navigate_ugv-ugv3 area44 area50 (1)
anon-navigate_ugv-ugv3 area50 area53 (1)
anon-navigate_ugv-ugv3 area53 area33 (1)
anon-navigate_ugv-ugv3 area33 area30 (1)
anon-navigate_ugv-ugv3 area30 area24 (1)
anon-navigate_ugv-ugv3 area24 area23 (1)
anon-navigate_ugv-ugv3 area23 area21 (1)
anon-drop_box-ugv3 ugv3store area21 box8 (1)
anon-navigate_ugv-ugv3 area21 area23 (1)
anon-navigate_ugv-ugv3 area23 area35 (1)
anon-navigate_ugv-ugv3 area35 area36 (1)
anon-navigate_ugv-ugv3 area36 area37 (1)
anon-navigate_ugv-ugv3 area37 area18 (1)
anon-navigate_ugv-ugv3 area18 area15 (1)
anon-navigate_ugv-ugv3 area15 area3 (1)
anon-navigate_ugv-ugv3 area3 area1 (1)
anon-pickup_box-ugv3 ugv3store cdm1 area1 box3 (1)
anon-navigate_ugv-ugv3 area1 area3 (1)
anon-navigate_ugv-ugv3 area3 area15 (1)
anon-navigate_ugv-ugv3 area15 area18 (1)
anon-navigate_ugv-ugv3 area18 area37 (1)
anon-navigate_ugv-ugv3 area37 area36 (1)
anon-navigate_ugv-ugv3 area36 area35 (1)
anon-navigate_ugv-ugv3 area35 area38 (1)
anon-navigate_ugv-ugv3 area38 area40 (1)
anon-navigate_ugv-ugv3 area40 area39 (1)
anon-drop_box-ugv3 ugv3store area39 box3 (1)
anon-navigate_ugv-ugv7 area31 area25 (1)
anon-navigate_ugv-ugv7 area25 area23 (1)
anon-navigate_ugv-ugv7 area23 area35 (1)
anon-navigate_ugv-ugv7 area35 area36 (1)
anon-navigate_ugv-ugv7 area36 area37 (1)
anon-navigate_ugv-ugv7 area37 area18 (1)
anon-navigate_ugv-ugv7 area18 area15 (1)
anon-navigate_ugv-ugv7 area15 area3 (1)
anon-navigate_ugv-ugv7 area3 area1 (1)
anon-pickup_box-ugv7 ugv7store cdm1 area1 box1 (1)
anon-navigate_ugv-ugv7 area1 area3 (1)
anon-navigate_ugv-ugv7 area3 area15 (1)
anon-navigate_ugv-ugv7 area15 area18 (1)
anon-navigate_ugv-ugv7 area18 area37 (1)
anon-navigate_ugv-ugv7 area37 area36 (1)
anon-navigate_ugv-ugv7 area36 area35 (1)
anon-navigate_ugv-ugv7 area35 area23 (1)
anon-navigate_ugv-ugv7 area23 area25 (1)
anon-navigate_ugv-ugv7 area25 area31 (1)
anon-navigate_ugv-ugv7 area31 area29 (1)
anon-drop_box-ugv7 ugv7store area29 box1 (1)
Plan length: 313 step(s).
Plan cost: 313
Initial state h value: 176.
Expanded 378 state(s).
Reopened 0 state(s).
Evaluated 379 state(s).
Evaluations: 758
Generated 80790 state(s).
Dead ends: 0 state(s).
Search time: 0.26s
Total time: 0.72s
Solution found.
Peak memory: 35896 KB
