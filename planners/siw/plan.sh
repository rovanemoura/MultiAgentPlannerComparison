#!/bin/bash
python map_lapkt.py planners/siw_plus-then-bfs_f/siw_plus_bfs_f.py ../benchmarks/unfactored/$1/$2/domain.pddl ../benchmarks/unfactored/$1/$2/problem.pddl ../plan.out
