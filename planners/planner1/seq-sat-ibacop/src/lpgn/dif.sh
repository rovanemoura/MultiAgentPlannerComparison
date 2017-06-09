#!/bin/sh
SRC=$1
HERE=$(pwd)
DEST=$HERE/diff.txt
if [ ! -d $SRC ]; then
echo "Dir $SRC not exists!!!"
exit
fi 

echo > $DEST
for DIR in include Parser BestFirst Utilities LocalSearch .; do
    cd $DIR 
    pwd
    if ls *.h &>/dev/null
	then
	EST="*.h"
    fi
    if ls *.c &>/dev/null
	then
	EST="*.c"
    fi	    

    for P in $EST; do
	echo >> diff.txt
	echo $P --------------------------------- >> $DEST
	if [ -d $SRC/$DIR ]; then
	    diff $P $SRC/$DIR/$P >> $DEST
	else
    	    diff $P $SRC/$P >> $DEST
	fi
	echo  >> $DEST
    done
    cd ..
done

