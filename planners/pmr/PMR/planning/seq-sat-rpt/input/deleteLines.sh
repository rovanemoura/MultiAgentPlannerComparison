#!/bin/bash

i=1

cd roverage

while [  $i -lt 10 ]; do
	sed -i.bak -e '5,10d;12d;24,30d' 0$i.soln
	let i=i+1
done

while [  $i -lt 21 ]; do
	sed -i.bak -e '5,10d;12d;24,30d' $i.soln
	let i=i+1
done
