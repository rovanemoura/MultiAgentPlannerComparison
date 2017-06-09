#! /bin/bash
# Anytime: wa* despues de la busqueda con lazy greedy best-first search
#cd /Users/dborrajo/planning/fd/
export PATH=/opt/local/bin:/opt/local/sbin:/bin:/usr/bin:/etc:/usr/sbin:/sbin:./:/usr/local/bin
\rm downward.tmp.* output output.sas plan_numbers_and_cost elapsed.time lama-output* $3 $4
./src/fast-downward.py --alias seq-sat-lama-2011 --plan-file $3 $1 $2 > $4

# ./src/translate/translate.py $1 $2
# ./src/preprocess/preprocess < output.sas
# ./src/search/downward ipc seq-sat-lama-2011 --plan-file $3 < output > $4
