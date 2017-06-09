#!/bin/bash

qac=""

tsac=""

ga="ac"

i=1

         while [  $i -lt 10 ]; do
		line=$(sed -n '2p' /home/nluis/pmr/mapr/result/unit-cost/rover/0$i.soln)
		cost=$(grep -o "\\([0-9][0-9]*\\)" <<<"$line")

		line=$(sed -n '2p' /home/nluis/pmr/mapr/result/unit-cost/rover/0$i.soln)
		timess=$(grep -o "\\([0-9][0-9]*.[0-9][0-9]*\\)" <<<"$line")

		qac+=$cost
		qac+="\t"	  

		tsac+=$timess
		tsac+="\t"

             let i=i+1 
         done

i=10
	 while [  $i -lt 21 ]; do
		line=$(sed -n '2p' /home/nluis/pmr/mapr/result/unit-cost/rover/$i.soln)
		cost=$(grep -o "\\([0-9][0-9]*\\)" <<<"$line")

		line=$(sed -n '2p' /home/nluis/pmr/mapr/result/unit-cost/rover/$i.soln)
		timess=$(grep -o "\\([0-9][0-9]*.[0-9][0-9]*\\)" <<<"$line")	

		qac+=$cost
		qac+="\t"	  

		tsac+=$timess
		tsac+="\t"

             let i=i+1 
         done

echo -e $qac

echo -e $tsac | sed s/\\./,/g

