#!/bin/bash

echo "LPG"

i=1

         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt2.sh ../../../ma-sat/roverage/domain/domain.pddl ../../../ma-sat/roverage/problems2/0$i.pddl rovag-$i-ac.soln.pp 1800"
	    # eval "./run-lpg-adapt2.sh ma-sat/zenotravelagb/domain/ma-zenotravel.pddl ma-sat/zenotravelagb/problems2/0$i.pddl zenag-$i-ac.soln.pp 1800"
	    # eval "./run-lpg-adapt2.sh ma-sat/satelliteag/domain/domain.pddl ma-sat/satelliteag/problems2/0$i.pddl satag-$i-ac.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt2.sh ../../../ma-sat/roverage/domain/domain.pddl ../../../ma-sat/roverage/problems2/$i.pddl rovag-$i-ac.soln.pp 1800"
	     #eval "./run-lpg-adapt2.sh ma-sat/zenotravelagb/domain/ma-zenotravel.pddl ma-sat/zenotravelagb/problems2/$i.pddl zenag-$i-ac.soln.pp 1800"
	    # eval "./run-lpg-adapt2.sh ma-sat/satelliteag/domain/domain.pddl ma-sat/satelliteag/problems2/$i.pddl satag-$i-ac.soln.pp 1800"
             let i=i+1 
         done
