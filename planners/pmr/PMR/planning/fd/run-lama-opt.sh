#! /bin/bash
# Anytime: wa* despues de la busqueda con lazy greedy best-first search
#cd /Users/dborrajo/planning/fd/
export PATH=/opt/local/bin:/opt/local/sbin:/bin:/usr/bin:/etc:/usr/sbin:/sbin:./:/usr/local/bin
\rm downward.tmp.* output output.sas plan_numbers_and_cost elapsed.time lama-output* $3 $4
./src/fast-downward.py --alias seq-opt-fdss-1 --plan-file $3 $1 $2 > $4


# ./src/translate/translate.py $1 $2
# ./src/preprocess/preprocess < output.sas
# ./src/search/downward ipc seq-opt-fdss-1 --plan-file $3 < output | grep "Plan length: \\|Plan cost: \\|Generated [0-9]* state\\|Total time:\\|^(" | sed -e "s/step(s).//" | sed -e "s/state(s).//" | sed -e "s/Plan length: / /" | sed -e "s/Plan cost: / /" | sed -e "s/Generated //" | sed -e "s/state(s)./ /" | sed -e "s/Total time: //" | sed -e "s/s//" > $4
# 
# Satisficing track
# =================
# 
#  - Fast Downward Autotune 1, satisficing version (seq-sat-fd-autotune-1)
#  - Fast Downward Autotune 2, satisficing version (seq-sat-fd-autotune-2)
#  - *Fast Downward Stone Soup 1, satisficing version (seq-sat-fdss-1)
#  - *Fast Downward Stone Soup 2, satisficing version (seq-sat-fdss-2)
#  - LAMA 2011 (seq-sat-lama-2011)
# 
# Optimization track
# ==================
# 
#  - BJOLP (seq-opt-bjolp)
#  - Fast Downward Autotune, optimizing version (seq-opt-fd-autotune)
#  - *Fast Downward Stone Soup 1, optimizing version (seq-opt-fdss-1)
#  - *Fast Downward Stone Soup 2, optimizing version (seq-opt-fdss-2)
#  - LM-Cut (seq-opt-lmcut)
#  - *Merge and Shrink (seq-opt-merge-and-shrink)
#  - Selective Max (seq-opt-selmax)
