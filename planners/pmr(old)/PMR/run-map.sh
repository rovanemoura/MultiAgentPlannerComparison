#!/bin/sh

usage()
{
cat << EOF
usage: $0
 -d <domain-file (absolute or relative path)>
 -p <problem-file (absolute or relative path)>
 -o <output-file (absolute or relative path)>
 [-i <init-problems>]
 -A <approach (centralized, cmap, mapr)>
 [-m <merging?>]
 [-s <sort-agents (name, random, maxgoals, mingoals)>]
 -g <goal-selection (subsets, all, all-achievable, rest-achievable, load-balance, best-cost, contract-net)>
 -a <algorithm (lama-unit-cost, lama-first, lama-seq, lama-opt, cgamer)>
 [-r <replanning-algorith (lama-first, lpg-adapt, errtplan)>]
 [-t <time (seconds)>]
 [-P <parallel-plan (nil, public, private)>]
 [-M <parallel-planning-p (t, nil)>]
 [-y <anytime-p (nil,t)>]
 [-Y <anytime-algorithm (lama-second) >]
 [-O <use-macros-p (nil,t)>]
 [-u <only-one-macro-p (nil,t)>]
 [-C <ma-pddl-p (nil,t)>]


examples:
 ./run-map.sh -d path_to_domain/domain.pddl -p path_to_problem/problem.pddl -o path_to_output/output -A cmap -s name -g rest-achievable -a lama-first -r lama-first -t 1800

./run-map.sh -d planning/sayphi/domains/rover/domain.pddl  -p planning/sayphi/domains/rover/probsets/ipc3-pfile07  -o kk -A cmap -s name -g rest-achievable -a lama-first -r lama-first -t 1800 -P public -M nil -y t -Y lama-second -O t

EOF
}

#falta incluir los parametros: (parallel-plan nil) (parallel-planning-p t)(anytime-p nil) (anytime-algorithm 'lama-second)

INIT_PROBLEMS="nil"
CENTRALIZED="nil"
CMAP="nil"
MAPR="nil"
MERGING="nil"
SORT="name"
GOAL="nil"
REPLANNING="nil"
TIME=1800
PARALLEL="nil"
MP="t"
ANYTIME="nil"
ANYTIMEA="lama-second"
MACROS="nil"
ONEMACRO="nil"
COM="nil"

while getopts :d:p:o:i:A:m:s:g:a:r:t:P:M:y:Y:O:u:C: option
do
  case "${option}" in
      d) DOMAIN=$(readlink -f ${OPTARG});;
      p) PROBLEM=$(readlink -f ${OPTARG});;
      o)
        if [ ! -f ${OPTARG} ]
        then
          touch ${OPTARG}
        fi
        OUTPUT=$(readlink -f ${OPTARG});;
      i)
        if [ -f ${OPTARG} ]
        then
          INIT_PROBLEMS=\"$(readlink -f ${OPTARG})\"
        fi;;
      A)
        case "${OPTARG}" in
          "centralized") CENTRALIZED="t";;
          "cmap") CMAP="t";;
          "mapr") MAPR="t";;
        esac
      ;;
      m) MERGING="${OPTARG}";;
      s) SORT=${OPTARG};;
      g) GOAL="'"${OPTARG};;
      a) ALGORITHM=${OPTARG};;
      r) REPLANNING="'"${OPTARG};;
      t) TIME=${OPTARG};;
      P)
         case "${OPTARG}" in
          "private") PARALLEL="'private";;
          "public")  PARALLEL="'public";;
         esac
      ;;
      M) MP=${OPTARG};;
      y) ANYTIME=${OPTARG};;
      Y) ANYTIMEA=${OPTARG};;
      O) MACROS=${OPTARG};;
      u) ONEMACRO=${OPTARG};;
      C) COM=${OPTARG};;
      ?)
        usage
        exit 1
      ;;
  esac
done

if [ -z $DOMAIN ] \
  || [ -z $PROBLEM ] \
  || [ -z $OUTPUT ] \
  || [ -z $ALGORITHM ]
then
  usage
  exit 1
fi

./map-core --eval "(progn (sb-ext:disable-debugger)(execute-map \"$DOMAIN\" \"$PROBLEM\" \"$OUTPUT\" :init-problems $INIT_PROBLEMS :run-original-centralized-p $CENTRALIZED :run-cmap-p $CMAP :run-mapr-p $MAPR :solve-for-merging-p $MERGING :sort-agents '$SORT :goal-selection $GOAL :algorithm '$ALGORITHM :replanning-algorithm $REPLANNING :timeout $TIME :parallel-plan $PARALLEL :parallel-planning-p $MP :anytime-p $ANYTIME :anytime-algorithm '$ANYTIMEA :use-macros-p $MACROS :only-one-macro-p $ONEMACRO :ma-pddl-p $COM) (pp-agents :ofile \"agents.lisp\") (with-open-file (str \"./filename.txt\"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (format str \"~a\" *ma-pddl-alist*)) (quit))"
