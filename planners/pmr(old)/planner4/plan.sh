#!/bin/bash 

# This script acts as an unified running interface of your planner (all planners will have similar
# script). During the competition this script will be used by our infrastructure to run your
# planner. In this testing environment, the run.sh script in your team's home dir calls this plan.sh
# to demonstrate running of your planner.
#
# parameters:   plan.sh <domain-name> <problem-name>
# example:    ./plan.sh logistics00   probLOGISTICS-5-0
#
# <domain-name>: name of the domain the planner should be run for; one of the dirs in team's home
#     benchmarks/factored/ or benchmarks/unfactored (your planner can use the factored, unfactored
#     or both input files)
#
# <problem-name>: name of the problem in the domain <domain-name> the planner should solve; one of
#     the dirs in team's home benchmarks/factored/<domain-name>/ or
#     benchmarks/unfactored/<domain-name>/ (your planner can use the factored, unfactored or both
#     input files)
#
# <output-file>: file the planner should write the resuting plan in
#
# Note: If your planner needs to run some other service(s) before, this is the right place to do it
# (e.g., message brokers, etc.). The running time is computed including this script.




# *************** REPLACE BY CODE RUNNING YOUR PLANNER ***************
#./mockup-planner ../benchmarks/unfactored/$1/$2/domain.pddl ../benchmarks/unfactored/$1/$2/problem.pddl ../plan.out

cd ../PMR
BASEDIR=${PWD}
FORMAT=""

mkdir -p ./temp
cp /home/rovane/planners/benchmarks/unfactored/$1/$2/domain.pddl ./temp
cp /home/rovane/planners/benchmarks/unfactored/$1/$2/problem.pddl ./temp


./run-map.sh -d $BASEDIR/temp/domain.pddl -p $BASEDIR/temp/problem.pddl -o $BASEDIR/temp/plan.out -A nil -m t -s mingoals -P nil -M t -a lama-first -r lama-first -g best-cost -y nil -Y lama-second -t 1800 -O t -u t -C t


if [ -f ./temp/plan.out.pp ]; then
./../VAL/validate $BASEDIR/temp/merged-obfuscated-domain.pddl $BASEDIR/temp/merged-obfuscated-problem.pddl $BASEDIR/temp/plan.out > $BASEDIR/temp/validate.txt


	if grep -q 'Successful plans:' $BASEDIR/temp/validate.txt; then
	cp $BASEDIR/temp/plan.out $BASEDIR/../plan.out
	FORMAT="ipc-num"
	else 
	cd
	cd $BASEDIR
	cd planning/seq-sat-rpt
	./plan_10000_0.3_0.3 $BASEDIR/temp/merged-obfuscated-domain.pddl $BASEDIR/temp/merged-obfuscated-problem.pddl $BASEDIR/temp/rpt_plan.out $BASEDIR/temp/plan.out $BASEDIR/temp/rpt_plan.txt
	
	cp $BASEDIR/temp/rpt_plan.out $BASEDIR/../plan.out
	FORMAT="ipc"
	fi
else
	cd
	cd $BASEDIR
	cd planning/fd/
	./run-lama-first.sh $BASEDIR/temp/merged-obfuscated-domain.pddl $BASEDIR/temp/merged-obfuscated-problem.pddl $BASEDIR/temp/plan_lf $BASEDIR/temp/out.txt
	cp $BASEDIR/temp/plan_lf $BASEDIR/../plan.out
	FORMAT="ipc"

	cd
fi

cd
cd $BASEDIR
	OUTP=../plan.out
	INP=../plan.out
	AG=$BASEDIR/agents.lisp
	DOM=$BASEDIR/temp/merged-obfuscated-domain.pddl
	PROB=$BASEDIR/temp/merged-obfuscated-problem.pddl


./map-core --eval "(progn (sb-ext:disable-debugger)
			(let* (
				(init-time (get-internal-real-time))
				(solution (case '$FORMAT (sol \"$INP\")
			        (ipc-num (solution-from-file \"$INP\"))
			        (ipc (read-plan-from-file \"$INP\"))))
			
			)

			(read-pddl-domain \"$DOM\")
       			(read-pddl-problem \"$PROB\")
			
			(setf *agents* (get-init-agents \"$AG\"))
			(setq parallel-plan
 	     			(mapcar #'(lambda(x) (cons (car x) (des-obfuscate (cdr x))))
 	             	(build-parallel-plan solution))
			)
			

				 (setq *ma-pddl-alist*
		   (with-open-file (infile \"filename.txt\" :direction :input :if-does-not-exist :ERROR)
				       (read infile nil 'eof))
				    )
				
				
				  (write-solution-in-output-file \"$OUTP\" parallel-plan nil nil nil nil nil nil
				     (elapsed-time init-time 'real-time) :ma-pddl-p t)
			
				)


		    (quit))"

	#./../VAL/validate ./temp/codmap-domain.pddl ./temp/codmap-problem.pddl ./../plan.out > ../result_pmr/val-$1-$2.txt
	#cp ../plan.out ../result_pmr/plan-$1-$2.out
rm -f $BASEDIR/filename.txt
rm -f $BASEDIR/agents.lisp
rm -rf $BASEDIR/temp
rm -f $BASEDIR/planning/LPG-adapt/plan.out_*

