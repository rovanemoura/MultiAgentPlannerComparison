#! /bin/bash

SCRIPT=$(readlink -f $0)
# Absolute path this script is in, thus /home/user/bin
SCRIPTPATH=$(dirname $SCRIPT)

cd $SCRIPTPATH

#cd /Users/dborrajo/planning/LPG-adapt
export PATH=/opt/local/bin:/opt/local/sbin:/bin:/usr/bin:/etc:/usr/sbin:/sbin:./:/usr/local/bin


rm  $3

./lpg-adapt speed -o $1 -f $2 -n 1 -cputime $4 -adapt_all_diff -out $3 >> /dev/null

cat $3_1.SOL | grep "\\(; Problem \\|^[0-9]\\+: \\|; Time\\|; Distance\\|; NrActions\\|; MetricValue\\)" | sed -e "s/[0-9][0-9]*:// " | sed -e "s/\[[0-9][0-9]*\]// " | sed -e "s/; Problem /( /" | sed -e "s/; Time \\([0-9][0-9]*.[0-9][0-9]*\\)/\\1 /" | sed -e "s/; NrActions \\([0-9][0-9]*\\)/\\1 /" | sed -e "s/; MetricValue \\([0-9][0-9]*.[0-9][0-9]*\\)/\\1 /" | sed -e "s/; Distance from input plan: \\([0-9][0-9]*\\)/ \\1 (/" > $3

echo "))" >> $3
