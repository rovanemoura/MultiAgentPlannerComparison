#!/usr/bin/python
from __future__ import print_function

import sys
import os

from ma_to_pddl import PlanningProblem

def main() :

	if len(sys.argv) < 5 :
		print( 'Missing arguments', file=sys.stderr )
		print( './map_lapkt.py <planner> <map domain> <map instance> <planfile>' )
		sys.exit(1)

	problem = PlanningProblem( sys.argv[2], sys.argv[3] )
	solver = sys.argv[1]

	classic_domain_file = 'k-domain.pddl'
	classic_problem_file = 'k-problem.pddl'
	plan_file = sys.argv[4]

	problem.write_pddl_domain( classic_domain_file )
	problem.write_pddl_problem( classic_problem_file )
	rv = os.system( 'python {0} {1} {2} {3}'.format( solver, classic_domain_file, classic_problem_file, plan_file ) )	

if __name__ == '__main__' :
	main()
