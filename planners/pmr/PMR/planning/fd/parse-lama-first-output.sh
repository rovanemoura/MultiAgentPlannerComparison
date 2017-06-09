#! /bin/bash

cat $1 | grep -a "Solution found!\\|no solution\\|Plan length: \\|Plan cost: \\|Generated [0-9]* state\\|Total time:\\|^(" | sed -e "s/step(s).//" | sed -e "s/state(s).//" | sed -e "s/Plan length: //" | sed -e "s/Plan cost: //" | sed -e "s/Generated //" | sed -e "s/state(s)./ /" | sed -e "s/Total time: //" | sed -e "s/Solution found!/T/"| sed -e "s/Completely explored state space -- no solution!/NIL/" | sed -e "s/s//" > $2
