#! /bin/bash
# Anytime: I assume there has already been a search with lama-unit-cost. So, now I continue searching for better plans
#cd /Users/dborrajo/planning/fd/
export PATH=/opt/local/bin:/opt/local/sbin:/bin:/usr/bin:/etc:/usr/sbin:/sbin:./:/usr/local/bin
\rm downward.tmp.* output output.sas plan_numbers_and_cost elapsed.time lama-output* $3 $4
./src/fast-downward.py --plan-file $3  $1 $2  --search-options --heuristic "hlm2,hff2=lm_ff_syn(lm_rhw(reasonable_orders=true,lm_cost_type=2,cost_type=2))" --search "iterated([lazy_greedy([hff2,hlm2],preferred=[hff2,hlm2]), lazy_wastar([hff2,hlm2],preferred=[hff2,hlm2],w=5), lazy_wastar([hff2,hlm2],preferred=[hff2,hlm2],w=3), lazy_wastar([hff2,hlm2],preferred=[hff2,hlm2],w=2), lazy_wastar([hff2,hlm2],preferred=[hff2,hlm2],w=1)], repeat_last=true,continue_on_fail=true,bound=$5)" > $4

# ./src/translate/translate.py $1 $2
# ./src/preprocess/preprocess < output.sas
# # ./src/search/downward ipc seq-sat-lama-2011 --plan-file $3 < output > $4
# ./src/search/downward --heuristic "hlm2,hff2=lm_ff_syn(lm_rhw(reasonable_orders=true,lm_cost_type=2,cost_type=2))" --search "iterated([lazy_greedy([hff2,hlm2],preferred=[hff2,hlm2]), lazy_wastar([hff2,hlm2],preferred=[hff2,hlm2],w=5), lazy_wastar([hff2,hlm2],preferred=[hff2,hlm2],w=3), lazy_wastar([hff2,hlm2],preferred=[hff2,hlm2],w=2), lazy_wastar([hff2,hlm2],preferred=[hff2,hlm2],w=1)], repeat_last=true,continue_on_fail=true,bound=$5)" --plan-file $3 < output > $4
# #                       -- plan-file $3 < output > $4
