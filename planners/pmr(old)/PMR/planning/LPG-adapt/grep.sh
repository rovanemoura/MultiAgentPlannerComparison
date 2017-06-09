#!/bin/sh
FILE=$1
shift
OPT=$*

for i in BestFirst Parser LocalSearch Utilities . ; do
    echo "--------------- $i ---------------"
    grep -n $OPT $FILE $i/*.c
done

echo "--------------- Header ---------------"
grep -n $OPT $FILE include/*.h
