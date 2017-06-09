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
FFD_PLANNER (){
    if [ $# -ne 4 ]
    then
        echo "Missing Parameters.... Quitting function FFD_PLANNER..."
        return 1
    fi
    pushd ~/TAPLAN/FFD/src
    rm -f sas_plan
    timeout -sKILL ${4}m python3 fast-downward.py $1  $2 --heuristic "hff=ff()" --heuristic "hcea=cea()"     --search "lazy_greedy([hff, hcea], preferred=[hff, hcea])"
    if [ $? -ne 0 ]
    then
        popd
        return 1
    fi
    cp sas_plan $3
    sed -i '/^;/d' $3
    popd
    return 0
}

IBACOP_PLANNER (){
    if [ $# -ne 4 ]
    then
        echo "Missing Parameters.... Quitting function FFD_PLANNER..."
        return 1
    fi
    rm -f ~/TAPLAN/seq-sat-ibacop/src/plans_folder/*
    timeout -sKILL ${4}m ~/TAPLAN/seq-sat-ibacop/plan $1  $2 $3 
    if [ $? -ne 0 ]
    then
        return 1
    fi
    return 0
}

FIRST_TIMEOUT=5 # If we cant solve the TA problem with in 5 mins switch planner
SECOND_TIMEOUT=10 # Run FFD with a larger time frame if second planner also failed to produce a plan in FIRST_TIMEOUT duration

domain_name=domain
problem_name=problem
#cleanup temporary file
mkdir -p input_folder
mkdir -p output_folder
mkdir -p ta_plan
mkdir -p small_problems/
rm -rf input_folder/*
rm -rf output_folder/*
rm -rf ta_plan/*
rm -rf small_problems/
rm -f  ~/TAPLAN/FFD_expander/src/final_plan.sol 

cp ../benchmarks/unfactored/$1/$2/${domain_name}.pddl input_folder/
cp ../benchmarks/unfactored/$1/$2/${problem_name}.pddl input_folder/

# Run updated version ma-to-pddl to obtain the compiles Transformation Agent (TA) Domain
# Currently assuming all problems are of type 1 RC and have fully connected state space (solvable by a single TA)
# Will try to incorporate tests for both and support for multiple TA problems in the future

python3 ma-to-pddl_TA.py input_folder $domain_name $problem_name output_folder

# Check if compilation was successful
if [ $? -ne 0 ]
then
    echo "Compilation step failed, Exiting the planner"
    exit 1
fi

# Run FFD Planner with initial timeout of 5 mins
FFD_PLANNER ~/TAPLAN/output_folder/${domain_name}.pddl  ~/TAPLAN/output_folder/${problem_name}_ta.pddl ~/TAPLAN/ta_plan/taplan.sol $FIRST_TIMEOUT
if [ $? -ne 0 ]
then
    IBACOP_PLANNER ~/TAPLAN/output_folder/${domain_name}.pddl  ~/TAPLAN/output_folder/${problem_name}_ta.pddl ~/TAPLAN/ta_plan/taplan.sol $FIRST_TIMEOUT
    if [ $? -ne 0 ]
    then
        FFD_PLANNER ~/TAPLAN/output_folder/${domain_name}.pddl  ~/TAPLAN/output_folder/${problem_name}_ta.pddl ~/TAPLAN/ta_plan/taplan.sol $SECOND_TIMEOUT
    else
        # Formatting IBACOP pln for expansion
        sed -i '/^;/d' ~/TAPLAN/ta_plan/taplan.sol
        sed -i 's/^.*\s(/(/' ~/TAPLAN/ta_plan/taplan.sol
        sed -i 's/)\s.*$/)/' ~/TAPLAN/ta_plan/taplan.sol
    fi
fi
if [ -f ~/TAPLAN/ta_plan/taplan.sol ]
then
    # Expand TA Plan to a multiagent plan 
    pushd ~/TAPLAN/FFD_expander/src/
    python3 fast-downward.py ~/TAPLAN/output_folder/${domain_name}.pddl  ~/TAPLAN/output_folder/${problem_name}.pddl ~/TAPLAN/ta_plan/taplan.sol  ~/TAPLAN/output_folder/${problem_name}.agents ~/TAPLAN/output_folder/${problem_name}.private ~/TAPLAN/output_folder/${problem_name}.ta_name  ~/TAPLAN/output_folder/${problem_name}.private_map ~/TAPLAN/output_folder/${problem_name}.agent_goals  --heuristic "hff=ff()" --heuristic "hcea=cea()"     --search "lazy_greedy([hff, hcea], preferred=[hff, hcea])"
    popd
else
    IBACOP_PLANNER ~/TAPLAN/output_folder/${domain_name}.pddl  ~/TAPLAN/output_folder/${problem_name}.pddl ~/TAPLAN/FFD_expander/src/final_plan.sol 30
fi

cp ~/TAPLAN/FFD_expander/src/final_plan.sol ../plan.out
