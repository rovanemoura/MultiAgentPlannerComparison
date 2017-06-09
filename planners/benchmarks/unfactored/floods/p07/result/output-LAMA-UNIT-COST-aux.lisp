INFO     Running translator.
INFO     translator inputs: ['/home/rovane/planners/benchmarks/unfactored/floods/p07/merged-obfuscated-domain.pddl', '/home/rovane/planners/benchmarks/unfactored/floods/p07/merged-obfuscated-problem.pddl']
INFO     translator arguments: []
Parsing...
Parsing: [0.010s CPU, 0.011s wall-clock]
Normalizing task... [0.000s CPU, 0.001s wall-clock]
Instantiating...
Generating Datalog program... [0.010s CPU, 0.003s wall-clock]
Normalizing Datalog program...
Normalizing Datalog program: [0.010s CPU, 0.012s wall-clock]
Preparing model... [0.000s CPU, 0.009s wall-clock]
Generated 297 rules.
Computing model... [0.180s CPU, 0.172s wall-clock]
8435 relevant atoms
3704 auxiliary atoms
12139 final queue length
20200 total queue pushes
Completing instantiation... [0.300s CPU, 0.290s wall-clock]
Instantiating: [0.500s CPU, 0.492s wall-clock]
Computing fact groups...
Finding invariants...
73 initial candidates
Finding invariants: [0.050s CPU, 0.053s wall-clock]
Checking invariant weight... [0.000s CPU, 0.001s wall-clock]
Instantiating groups... [0.010s CPU, 0.012s wall-clock]
Collecting mutex groups... [0.000s CPU, 0.001s wall-clock]
Choosing groups...
495 uncovered facts
Choosing groups: [0.000s CPU, 0.003s wall-clock]
Building translation key... [0.000s CPU, 0.003s wall-clock]
Computing fact groups: [0.080s CPU, 0.085s wall-clock]
Building STRIPS to SAS dictionary... [0.000s CPU, 0.001s wall-clock]
Building dictionary for full mutex groups... [0.000s CPU, 0.001s wall-clock]
Building mutex information...
Building mutex information: [0.000s CPU, 0.001s wall-clock]
Translating task...
Processing axioms...
Simplifying axioms... [0.000s CPU, 0.000s wall-clock]
Processing axioms: [0.020s CPU, 0.012s wall-clock]
Translating task: [0.310s CPU, 0.306s wall-clock]
4597 effect conditions simplified
0 implied preconditions added
Detecting unreachable propositions...
0 operators removed
36 propositions removed
Detecting unreachable propositions: [0.060s CPU, 0.059s wall-clock]
Translator variables: 504
Translator derived variables: 0
Translator facts: 1745
Translator goal facts: 27
Translator mutex groups: 58
Translator total mutex groups size: 853
Translator operators: 6648
Translator axioms: 0
Translator task size: 34107
Translator peak memory: 59860 KB
Writing output... [0.060s CPU, 0.057s wall-clock]
Done! [1.030s CPU, 1.030s wall-clock]
INFO     Running preprocessor.
INFO     preprocessor input: output.sas
INFO     preprocessor arguments: []
Building causal graph...
The causal graph is not acyclic.
303 variables of 504 necessary
0 of 58 mutex groups necessary.
6648 of 6648 operators necessary.
0 of 0 axiom rules necessary.
Building domain transition graphs...
solveable in poly time 0
Building successor generator...
Preprocessor facts: 1343
Preprocessor derived variables: 0
Preprocessor task size: 29283
Writing output...
done
INFO     Running search.
INFO     search input: output
INFO     search executable: /home/rovane/planners/cmap/planning/fd/src/search/downward-release
INFO     search arguments: ['--heuristic', 'hlm,hff=lm_ff_syn(lm_rhw(reasonable_orders=true,lm_cost_type=1,cost_type=1))', '--search', 'lazy_greedy(hff,hlm,preferred=[hff,hlm])', '--internal-plan-file', '/home/rovane/planners/benchmarks/unfactored/floods/p07/result/plan-LAMA-UNIT-COST.lisp']
reading input... [t=0s]
Simplifying transitions... done!
done reading input! [t=0.12s]
building causal graph...done! [t=0.12s]
packing state variables...done! [t=0.12s]
Variables: 303
Facts: 1343
Bytes per state: 52
done initalizing global data [t=0.12s]
Initializing Exploration...
Generating landmarks using the RPG/SAS+ approach
approx. reasonable orders
approx. obedient reasonable orders
Removed 0 reasonable or obedient reasonable orders
Landmarks generation time: 0.0452032s
Discovered 54 landmarks, of which 0 are disjunctive and 0 are conjunctive 
27 edges
Initializing LAMA-FF Synergy Object
Initializing landmarks count heuristic...
27 initial landmarks, 27 goal landmarks
Conducting lazy best first search, (real) bound = 2147483647
Best heuristic value: 116 [g=0, 1 evaluated, 0 expanded, t=0.16s, 16968 KB]
Best heuristic value: 115 [g=2, 4 evaluated, 3 expanded, t=0.16s, 16968 KB]
Best heuristic value: 114 [g=3, 5 evaluated, 4 expanded, t=0.16s, 16968 KB]
Best heuristic value: 113 [g=5, 8 evaluated, 7 expanded, t=0.16s, 16968 KB]
Best heuristic value: 112 [g=6, 9 evaluated, 8 expanded, t=0.16s, 16968 KB]
Best heuristic value: 111 [g=7, 10 evaluated, 9 expanded, t=0.16s, 16968 KB]
Best heuristic value: 110 [g=9, 12 evaluated, 11 expanded, t=0.16s, 16968 KB]
Best heuristic value: 109 [g=10, 13 evaluated, 12 expanded, t=0.16s, 16968 KB]
Best heuristic value: 108 [g=11, 14 evaluated, 13 expanded, t=0.16s, 16968 KB]
Best heuristic value: 107 [g=12, 15 evaluated, 14 expanded, t=0.17s, 16968 KB]
Best heuristic value: 106 [g=13, 16 evaluated, 15 expanded, t=0.17s, 16968 KB]
Best heuristic value: 105 [g=14, 17 evaluated, 16 expanded, t=0.17s, 16968 KB]
Best heuristic value: 103 [g=17, 21 evaluated, 20 expanded, t=0.17s, 16968 KB]
Best heuristic value: 102 [g=18, 22 evaluated, 21 expanded, t=0.17s, 16968 KB]
Best heuristic value: 100 [g=21, 25 evaluated, 24 expanded, t=0.17s, 16968 KB]
Best heuristic value: 99 [g=22, 26 evaluated, 25 expanded, t=0.17s, 16968 KB]
Best heuristic value: 97 [g=25, 29 evaluated, 28 expanded, t=0.17s, 16968 KB]
Best heuristic value: 96 [g=26, 30 evaluated, 29 expanded, t=0.17s, 16968 KB]
Best heuristic value: 94 [g=29, 33 evaluated, 32 expanded, t=0.17s, 16968 KB]
Best heuristic value: 93 [g=30, 34 evaluated, 33 expanded, t=0.17s, 16968 KB]
Best heuristic value: 91 [g=33, 37 evaluated, 36 expanded, t=0.17s, 16968 KB]
Best heuristic value: 90 [g=34, 38 evaluated, 37 expanded, t=0.17s, 16968 KB]
Best heuristic value: 89 [g=35, 39 evaluated, 38 expanded, t=0.17s, 16968 KB]
Best heuristic value: 88 [g=36, 40 evaluated, 39 expanded, t=0.17s, 16968 KB]
Best heuristic value: 87 [g=37, 41 evaluated, 40 expanded, t=0.18s, 16968 KB]
Best heuristic value: 85 [g=40, 45 evaluated, 44 expanded, t=0.18s, 16980 KB]
Best heuristic value: 84 [g=41, 46 evaluated, 45 expanded, t=0.18s, 16980 KB]
Best heuristic value: 83 [g=43, 48 evaluated, 47 expanded, t=0.18s, 16980 KB]
Best heuristic value: 82 [g=44, 49 evaluated, 48 expanded, t=0.18s, 16980 KB]
Best heuristic value: 81 [g=45, 50 evaluated, 49 expanded, t=0.18s, 16980 KB]
Best heuristic value: 80 [g=49, 54 evaluated, 53 expanded, t=0.18s, 16980 KB]
Best heuristic value: 79 [g=50, 55 evaluated, 54 expanded, t=0.18s, 16980 KB]
Best heuristic value: 78 [g=51, 56 evaluated, 55 expanded, t=0.18s, 16980 KB]
Best heuristic value: 77 [g=52, 57 evaluated, 56 expanded, t=0.18s, 16980 KB]
Best heuristic value: 76 [g=53, 58 evaluated, 57 expanded, t=0.18s, 16980 KB]
Best heuristic value: 73 [g=54, 60 evaluated, 59 expanded, t=0.18s, 16980 KB]
Best heuristic value: 72 [g=55, 61 evaluated, 60 expanded, t=0.18s, 16980 KB]
Best heuristic value: 71 [g=56, 62 evaluated, 61 expanded, t=0.18s, 16980 KB]
Best heuristic value: 70 [g=57, 63 evaluated, 62 expanded, t=0.18s, 16980 KB]
Best heuristic value: 69 [g=81, 100 evaluated, 99 expanded, t=0.19s, 16980 KB]
Best heuristic value: 65 [g=83, 102 evaluated, 101 expanded, t=0.19s, 16980 KB]
Best heuristic value: 64 [g=85, 105 evaluated, 104 expanded, t=0.19s, 16980 KB]
Best heuristic value: 63 [g=89, 111 evaluated, 110 expanded, t=0.2s, 16980 KB]
Best heuristic value: 62 [g=90, 112 evaluated, 111 expanded, t=0.2s, 16980 KB]
Best heuristic value: 61 [g=91, 113 evaluated, 112 expanded, t=0.2s, 16980 KB]
Best heuristic value: 60 [g=92, 115 evaluated, 114 expanded, t=0.2s, 16980 KB]
Best heuristic value: 59 [g=93, 116 evaluated, 115 expanded, t=0.2s, 16980 KB]
Best heuristic value: 58 [g=94, 117 evaluated, 116 expanded, t=0.2s, 16980 KB]
Best heuristic value: 57 [g=95, 118 evaluated, 117 expanded, t=0.2s, 16980 KB]
Best heuristic value: 56 [g=96, 119 evaluated, 118 expanded, t=0.2s, 16980 KB]
Best heuristic value: 55 [g=97, 120 evaluated, 119 expanded, t=0.2s, 16980 KB]
Best heuristic value: 54 [g=98, 121 evaluated, 120 expanded, t=0.2s, 16980 KB]
Best heuristic value: 52 [g=102, 127 evaluated, 126 expanded, t=0.2s, 16980 KB]
Best heuristic value: 50 [g=105, 132 evaluated, 131 expanded, t=0.2s, 17112 KB]
Best heuristic value: 49 [g=106, 133 evaluated, 132 expanded, t=0.2s, 17112 KB]
Best heuristic value: 45 [g=107, 134 evaluated, 133 expanded, t=0.2s, 17112 KB]
Best heuristic value: 43 [g=119, 150 evaluated, 149 expanded, t=0.2s, 17112 KB]
Best heuristic value: 41 [g=128, 166 evaluated, 165 expanded, t=0.21s, 17112 KB]
Best heuristic value: 40 [g=130, 172 evaluated, 171 expanded, t=0.21s, 17112 KB]
Best heuristic value: 39 [g=131, 173 evaluated, 172 expanded, t=0.21s, 17112 KB]
Best heuristic value: 38 [g=132, 174 evaluated, 173 expanded, t=0.21s, 17112 KB]
Best heuristic value: 37 [g=133, 175 evaluated, 174 expanded, t=0.21s, 17112 KB]
Best heuristic value: 36 [g=134, 176 evaluated, 175 expanded, t=0.21s, 17112 KB]
Best heuristic value: 35 [g=135, 177 evaluated, 176 expanded, t=0.21s, 17112 KB]
Best heuristic value: 34 [g=136, 178 evaluated, 177 expanded, t=0.21s, 17112 KB]
Best heuristic value: 33 [g=144, 187 evaluated, 186 expanded, t=0.22s, 17112 KB]
Best heuristic value: 32 [g=145, 188 evaluated, 187 expanded, t=0.22s, 17112 KB]
Best heuristic value: 31 [g=146, 189 evaluated, 188 expanded, t=0.22s, 17112 KB]
Best heuristic value: 30 [g=147, 190 evaluated, 189 expanded, t=0.22s, 17112 KB]
Best heuristic value: 29 [g=148, 191 evaluated, 190 expanded, t=0.22s, 17112 KB]
Best heuristic value: 28 [g=149, 192 evaluated, 191 expanded, t=0.22s, 17112 KB]
Best heuristic value: 27 [g=150, 193 evaluated, 192 expanded, t=0.22s, 17112 KB]
Best heuristic value: 26 [g=151, 194 evaluated, 193 expanded, t=0.22s, 17112 KB]
Best heuristic value: 25 [g=152, 195 evaluated, 194 expanded, t=0.22s, 17112 KB]
Best heuristic value: 24 [g=154, 198 evaluated, 197 expanded, t=0.22s, 17112 KB]
Best heuristic value: 23 [g=157, 203 evaluated, 202 expanded, t=0.22s, 17112 KB]
Best heuristic value: 22 [g=158, 204 evaluated, 203 expanded, t=0.22s, 17112 KB]
Best heuristic value: 21 [g=159, 205 evaluated, 204 expanded, t=0.22s, 17112 KB]
Best heuristic value: 20 [g=160, 206 evaluated, 205 expanded, t=0.22s, 17112 KB]
Best heuristic value: 19 [g=161, 207 evaluated, 206 expanded, t=0.22s, 17112 KB]
Best heuristic value: 18 [g=162, 208 evaluated, 207 expanded, t=0.22s, 17112 KB]
Best heuristic value: 17 [g=173, 229 evaluated, 228 expanded, t=0.22s, 17244 KB]
Best heuristic value: 16 [g=174, 230 evaluated, 229 expanded, t=0.22s, 17244 KB]
Best heuristic value: 15 [g=175, 231 evaluated, 230 expanded, t=0.22s, 17244 KB]
Best heuristic value: 14 [g=176, 232 evaluated, 231 expanded, t=0.23s, 17244 KB]
Best heuristic value: 13 [g=177, 233 evaluated, 232 expanded, t=0.23s, 17244 KB]
Best heuristic value: 12 [g=178, 234 evaluated, 233 expanded, t=0.23s, 17244 KB]
Best heuristic value: 11 [g=179, 235 evaluated, 234 expanded, t=0.23s, 17244 KB]
Best heuristic value: 10 [g=183, 243 evaluated, 242 expanded, t=0.23s, 17244 KB]
Best heuristic value: 9 [g=184, 244 evaluated, 243 expanded, t=0.23s, 17244 KB]
Best heuristic value: 8 [g=185, 245 evaluated, 244 expanded, t=0.23s, 17244 KB]
Best heuristic value: 7 [g=186, 246 evaluated, 245 expanded, t=0.23s, 17244 KB]
Best heuristic value: 6 [g=187, 247 evaluated, 246 expanded, t=0.23s, 17244 KB]
Best heuristic value: 5 [g=188, 248 evaluated, 247 expanded, t=0.23s, 17244 KB]
Best heuristic value: 4 [g=189, 249 evaluated, 248 expanded, t=0.23s, 17244 KB]
Best heuristic value: 3 [g=190, 250 evaluated, 249 expanded, t=0.23s, 17244 KB]
Best heuristic value: 2 [g=191, 251 evaluated, 250 expanded, t=0.23s, 17244 KB]
Best heuristic value: 1 [g=192, 252 evaluated, 251 expanded, t=0.23s, 17244 KB]
Solution found!
Actual search time: 0.07s [t=0.23s]
anon-pickup_box-ugv7 ugv7store cdm4 area33 box6 (1)
anon-navigate_ugv-ugv7 area33 area44 (1)
anon-drop_box-ugv7 ugv7store area44 box6 (1)
anon-navigate_uav-uav8 area33 area34 (1)
anon-take_picture-uav8 area34 disaster9 (1)
anon-navigate_uav-uav8 area34 area9 (1)
anon-communicate_data-uav8 cdm2 disaster9 area9 area13 (1)
anon-navigate_uav-uav8 area9 area39 (1)
anon-take_picture-uav8 area39 disaster8 (1)
anon-navigate_uav-uav8 area39 area9 (1)
anon-communicate_data-uav8 cdm2 disaster8 area9 area13 (1)
anon-navigate_uav-uav8 area9 area10 (1)
anon-take_picture-uav8 area10 disaster7 (1)
anon-communicate_data-uav8 cdm2 disaster7 area10 area13 (1)
anon-navigate_uav-uav8 area10 area25 (1)
anon-take_picture-uav8 area25 disaster6 (1)
anon-navigate_uav-uav8 area25 area9 (1)
anon-communicate_data-uav8 cdm2 disaster6 area9 area13 (1)
anon-navigate_uav-uav8 area9 area32 (1)
anon-take_picture-uav8 area32 disaster5 (1)
anon-navigate_uav-uav8 area32 area9 (1)
anon-communicate_data-uav8 cdm2 disaster5 area9 area13 (1)
anon-navigate_uav-uav8 area9 area18 (1)
anon-take_picture-uav8 area18 disaster4 (1)
anon-navigate_uav-uav8 area18 area9 (1)
anon-communicate_data-uav8 cdm2 disaster4 area9 area13 (1)
anon-navigate_uav-uav8 area9 area28 (1)
anon-take_picture-uav8 area28 disaster3 (1)
anon-navigate_uav-uav8 area28 area9 (1)
anon-communicate_data-uav8 cdm2 disaster3 area9 area13 (1)
anon-navigate_uav-uav8 area9 area17 (1)
anon-take_picture-uav8 area17 disaster2 (1)
anon-navigate_uav-uav8 area17 area9 (1)
anon-communicate_data-uav8 cdm2 disaster2 area9 area13 (1)
anon-navigate_uav-uav8 area9 area23 (1)
anon-take_picture-uav8 area23 disaster11 (1)
anon-communicate_data-uav8 cdm3 disaster11 area23 area21 (1)
anon-navigate_uav-uav8 area23 area40 (1)
anon-take_picture-uav8 area40 disaster10 (1)
anon-navigate_uav-uav8 area40 area9 (1)
anon-communicate_data-uav8 cdm2 disaster10 area9 area13 (1)
anon-navigate_uav-uav8 area9 area11 (1)
anon-take_picture-uav8 area11 disaster1 (1)
anon-navigate_uav-uav8 area11 area9 (1)
anon-communicate_data-uav8 cdm2 disaster1 area9 area13 (1)
anon-navigate_usv-usv6 area21 area36 (1)
anon-navigate_usv-usv6 area36 area38 (1)
anon-navigate_usv-usv6 area38 area17 (1)
anon-sample_water-usv6 usv6store area17 (1)
anon-navigate_usv-usv6 area17 area38 (1)
anon-navigate_usv-usv6 area38 area36 (1)
anon-navigate_usv-usv6 area36 area21 (1)
anon-drop_sample-usv6 usv6store area21 area17 cdm3 (1)
anon-pickup_box-ugv3 ugv3store cdm2 area13 box2 (1)
anon-navigate_ugv-ugv3 area13 area10 (1)
anon-navigate_ugv-ugv3 area10 area7 (1)
anon-navigate_ugv-ugv3 area7 area28 (1)
anon-drop_box-ugv3 ugv3store area28 box2 (1)
anon-navigate_usv-usv4 area13 area9 (1)
anon-navigate_usv-usv4 area9 area8 (1)
anon-sample_water-usv4 usv4store area8 (1)
anon-navigate_usv-usv4 area8 area9 (1)
anon-navigate_usv-usv4 area9 area13 (1)
anon-navigate_usv-usv4 area13 area44 (1)
anon-navigate_usv-usv4 area44 area33 (1)
anon-drop_sample-usv4 usv4store area33 area8 cdm4 (1)
anon-pickup_box-ugv5 ugv5store cdm3 area21 box5 (1)
anon-navigate_ugv-ugv5 area21 area36 (1)
anon-navigate_ugv-ugv5 area36 area37 (1)
anon-navigate_ugv-ugv5 area37 area18 (1)
anon-navigate_ugv-ugv5 area18 area20 (1)
anon-drop_box-ugv5 ugv5store area20 box5 (1)
anon-navigate_ugv-ugv7 area44 area43 (1)
anon-navigate_ugv-ugv7 area43 area41 (1)
anon-pickup_box-ugv7 ugv7store cdm5 area41 box7 (1)
anon-navigate_ugv-ugv7 area41 area43 (1)
anon-navigate_ugv-ugv7 area43 area44 (1)
anon-navigate_ugv-ugv7 area44 area33 (1)
anon-navigate_ugv-ugv7 area33 area30 (1)
anon-drop_box-ugv7 ugv7store area30 box7 (1)
anon-navigate_usv-usv4 area33 area29 (1)
anon-navigate_usv-usv4 area29 area24 (1)
anon-navigate_usv-usv4 area24 area22 (1)
anon-navigate_usv-usv8 area33 area44 (1)
anon-sample_water-usv8 usv8store area44 (1)
anon-navigate_usv-usv8 area44 area33 (1)
anon-navigate_usv-usv8 area33 area29 (1)
anon-navigate_usv-usv8 area29 area24 (1)
anon-navigate_usv-usv8 area24 area22 (1)
anon-navigate_usv-usv8 area22 area21 (1)
anon-drop_sample-usv8 usv8store area21 area44 cdm3 (1)
anon-pickup_box-ugv1 ugv1store cdm1 area1 box3 (1)
anon-navigate_ugv-ugv1 area1 area3 (1)
anon-navigate_ugv-ugv1 area3 area15 (1)
anon-navigate_ugv-ugv1 area15 area18 (1)
anon-navigate_ugv-ugv1 area18 area37 (1)
anon-navigate_ugv-ugv1 area37 area34 (1)
anon-drop_box-ugv1 ugv1store area34 box3 (1)
anon-navigate_usv-usv2 area1 area2 (1)
anon-navigate_usv-usv2 area2 area6 (1)
anon-sample_water-usv2 usv2store area6 (1)
anon-navigate_usv-usv2 area6 area2 (1)
anon-navigate_usv-usv2 area2 area4 (1)
anon-navigate_usv-usv2 area4 area9 (1)
anon-navigate_usv-usv2 area9 area13 (1)
anon-drop_sample-usv2 usv2store area13 area6 cdm2 (1)
anon-navigate_usv-usv2 area13 area9 (1)
anon-navigate_usv-usv2 area9 area4 (1)
anon-navigate_usv-usv2 area4 area2 (1)
anon-sample_water-usv2 usv2store area2 (1)
anon-navigate_usv-usv2 area2 area4 (1)
anon-navigate_usv-usv2 area4 area9 (1)
anon-navigate_usv-usv2 area9 area13 (1)
anon-navigate_usv-usv2 area13 area44 (1)
anon-navigate_usv-usv2 area44 area42 (1)
anon-navigate_usv-usv2 area42 area41 (1)
anon-drop_sample-usv2 usv2store area41 area2 cdm5 (1)
anon-sample_water-usv2 usv2store area41 (1)
anon-navigate_usv-usv2 area41 area42 (1)
anon-navigate_usv-usv2 area42 area44 (1)
anon-navigate_usv-usv2 area44 area13 (1)
anon-navigate_usv-usv2 area13 area9 (1)
anon-navigate_usv-usv2 area9 area4 (1)
anon-navigate_usv-usv2 area4 area2 (1)
anon-navigate_usv-usv2 area2 area1 (1)
anon-drop_sample-usv2 usv2store area1 area41 cdm1 (1)
anon-navigate_usv-usv4 area22 area34 (1)
anon-sample_water-usv4 usv4store area34 (1)
anon-navigate_usv-usv4 area34 area22 (1)
anon-navigate_usv-usv4 area22 area24 (1)
anon-navigate_usv-usv4 area24 area29 (1)
anon-navigate_usv-usv4 area29 area33 (1)
anon-navigate_usv-usv4 area33 area44 (1)
anon-navigate_usv-usv4 area44 area42 (1)
anon-navigate_usv-usv4 area42 area41 (1)
anon-drop_sample-usv4 usv4store area41 area34 cdm5 (1)
anon-navigate_usv-usv2 area1 area2 (1)
anon-navigate_usv-usv2 area2 area4 (1)
anon-navigate_usv-usv2 area4 area9 (1)
anon-navigate_usv-usv2 area9 area8 (1)
anon-navigate_usv-usv2 area8 area27 (1)
anon-navigate_usv-usv2 area27 area28 (1)
anon-navigate_usv-usv2 area28 area29 (1)
anon-sample_water-usv2 usv2store area29 (1)
anon-navigate_usv-usv2 area29 area33 (1)
anon-navigate_usv-usv2 area33 area44 (1)
anon-navigate_usv-usv2 area44 area13 (1)
anon-navigate_usv-usv2 area13 area9 (1)
anon-navigate_usv-usv2 area9 area4 (1)
anon-navigate_usv-usv2 area4 area2 (1)
anon-navigate_usv-usv2 area2 area1 (1)
anon-drop_sample-usv2 usv2store area1 area29 cdm1 (1)
anon-navigate_ugv-ugv3 area28 area27 (1)
anon-navigate_ugv-ugv3 area27 area30 (1)
anon-navigate_ugv-ugv3 area30 area33 (1)
anon-pickup_box-ugv3 ugv3store cdm4 area33 box4 (1)
anon-navigate_ugv-ugv3 area33 area44 (1)
anon-navigate_ugv-ugv3 area44 area13 (1)
anon-navigate_ugv-ugv3 area13 area10 (1)
anon-navigate_ugv-ugv3 area10 area4 (1)
anon-navigate_ugv-ugv3 area4 area6 (1)
anon-drop_box-ugv3 ugv3store area6 box4 (1)
anon-navigate_ugv-ugv7 area30 area33 (1)
anon-navigate_ugv-ugv7 area33 area44 (1)
anon-navigate_ugv-ugv7 area44 area13 (1)
anon-navigate_ugv-ugv7 area13 area10 (1)
anon-navigate_ugv-ugv7 area10 area4 (1)
anon-navigate_ugv-ugv7 area4 area3 (1)
anon-navigate_ugv-ugv7 area3 area1 (1)
anon-pickup_box-ugv7 ugv7store cdm1 area1 box1 (1)
anon-navigate_ugv-ugv7 area1 area3 (1)
anon-navigate_ugv-ugv7 area3 area15 (1)
anon-navigate_ugv-ugv7 area15 area18 (1)
anon-navigate_ugv-ugv7 area18 area37 (1)
anon-navigate_ugv-ugv7 area37 area36 (1)
anon-navigate_ugv-ugv7 area36 area35 (1)
anon-navigate_ugv-ugv7 area35 area23 (1)
anon-navigate_ugv-ugv7 area23 area24 (1)
anon-drop_box-ugv7 ugv7store area24 box1 (1)
anon-navigate_ugv-ugv7 area24 area23 (1)
anon-navigate_ugv-ugv7 area23 area35 (1)
anon-navigate_ugv-ugv7 area35 area38 (1)
anon-pickup_box-ugv9 ugv9store cdm5 area41 box8 (1)
anon-navigate_ugv-ugv9 area41 area43 (1)
anon-navigate_ugv-ugv9 area43 area44 (1)
anon-navigate_ugv-ugv9 area44 area33 (1)
anon-navigate_ugv-ugv9 area33 area30 (1)
anon-navigate_ugv-ugv9 area30 area24 (1)
anon-navigate_ugv-ugv9 area24 area23 (1)
anon-navigate_ugv-ugv9 area23 area35 (1)
anon-navigate_ugv-ugv9 area35 area38 (1)
anon-navigate_ugv-ugv9 area38 area40 (1)
anon-drop_box-ugv9 ugv9store area40 box8 (1)
Plan length: 193 step(s).
Plan cost: 193
Initial state h value: 116.
Expanded 252 state(s).
Reopened 0 state(s).
Evaluated 253 state(s).
Evaluations: 506
Generated 17631 state(s).
Dead ends: 0 state(s).
Search time: 0.07s
Total time: 0.23s
Solution found.
Peak memory: 17244 KB
