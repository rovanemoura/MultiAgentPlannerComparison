#!/bin/bash

qac=""

tsac=""

ga="ac"

i=1

         while [  $i -lt 10 ]; do
     	 line=$(tail -1 /home/nluis/pmr/mapr/planning/LPG-adapt/results/zenag-$i-$ga.soln.pp_1.SOL | sed -e 's/^\(.\{2\}\).*$/\1/') 
		 cost=$(grep -o "\\([0-9][0-9]*\\)" <<<"$line")
	
		liness=$(sed -n '7p' /home/nluis/pmr/mapr/planning/LPG-adapt/results/zenag-$i-$ga.soln.pp_1.SOL)
		timess=$(grep -o "\\([0-9][0-9]*.[0-9][0-9]*\\)" <<<"$liness")	

		qac+=$cost
		qac+="\t"	  

		tsac+=$timess
		tsac+="\t"

             let i=i+1 
         done

i=10
	 while [  $i -lt 21 ]; do
     	 line=$(tail -1 /home/nluis/pmr/mapr/planning/LPG-adapt/results/zenag-$i-$ga.soln.pp_1.SOL | sed -e 's/^\(.\{2\}\).*$/\1/') 
		 cost=$(grep -o "\\([0-9][0-9]*\\)" <<<"$line")
	
		liness=$(sed -n '7p' /home/nluis/pmr/mapr/planning/LPG-adapt/results/zenag-$i-$ga.soln.pp_1.SOL)
		timess=$(grep -o "\\([0-9][0-9]*.[0-9][0-9]*\\)" <<<"$liness")	

		qac+=$cost
		qac+="\t"	  

		tsac+=$timess
		tsac+="\t"

             let i=i+1 
         done

echo -e $qac

echo -e $tsac | sed s/\\./,/g

