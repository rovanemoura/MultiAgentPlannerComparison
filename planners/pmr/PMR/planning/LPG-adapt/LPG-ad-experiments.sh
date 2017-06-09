#!/bin/bash

echo "MERGING STARTS"


i=2
       echo "Executing problem $i"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-5.pddl $i-ac-05.soln.pp input/merging/port/all-achievable/0$i/0$i-05.soln.pp 1800"
 	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-10.pddl $i-ac-10.soln.pp input/merging/port/all-achievable/0$i/0$i-10.soln.pp 1800"    
 	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-20.pddl $i-ac-20.soln.pp input/merging/port/all-achievable/0$i/0$i-20.soln.pp 1800"   
 	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-30.pddl $i-ac-30.soln.pp input/merging/port/all-achievable/0$i/0$i-30.soln.pp 1800"    		 

echo "ALL-ACHIEVABLE"
i=5
     while [ $i -lt 10 ]; do
       echo "Executing problem $i"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-5.pddl $i-ac-05.soln.pp input/merging/port/all-achievable/0$i/0$i-05.soln.pp 1800"
 	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-10.pddl $i-ac-10.soln.pp input/merging/port/all-achievable/0$i/0$i-10.soln.pp 1800"    
 	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-20.pddl $i-ac-20.soln.pp input/merging/port/all-achievable/0$i/0$i-20.soln.pp 1800"   
 	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-30.pddl $i-ac-30.soln.pp input/merging/port/all-achievable/0$i/0$i-30.soln.pp 1800"    		 
		 let i=i+5 
     done
i=10
     while [ $i -lt 21 ]; do
       echo "Executing problem $i"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-$i-10.pddl $i-ac-10.soln.pp input/merging/port/all-achievable/$i/$i-10.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-$i-20.pddl $i-ac-20.soln.pp input/merging/port/all-achievable/$i/$i-20.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-$i-30.pddl $i-ac-30.soln.pp input/merging/port/all-achievable/$i/$i-30.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-$i-40.pddl $i-ac-40.soln.pp input/merging/port/all-achievable/$i/$i-40.soln.pp 1800"
		 let i=i+5 
     done

echo "BEST-COST"    
i=2
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-5.pddl $i-bc-05.soln.pp input/merging/port/best-cost/0$i/0$i-05.soln.pp 1800"
 	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-10.pddl $i-bc-10.soln.pp input/merging/port/best-cost/0$i/0$i-10.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-20.pddl $i-bc-20.soln.pp input/merging/port/best-cost/0$i/0$i-20.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-30.pddl $i-bc-30.soln.pp input/merging/port/best-cost/0$i/0$i-30.soln.pp 1800"
      
i=5

     while [ $i -lt 10 ]; do
       echo "Executing problem $i"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-5.pddl $i-bc-05.soln.pp input/merging/port/best-cost/0$i/0$i-05.soln.pp 1800"
 	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-10.pddl $i-bc-10.soln.pp input/merging/port/best-cost/0$i/0$i-10.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-20.pddl $i-bc-20.soln.pp input/merging/port/best-cost/0$i/0$i-20.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-30.pddl $i-bc-30.soln.pp input/merging/port/best-cost/0$i/0$i-30.soln.pp 1800"
      let i=i+5 
     done
i=10
     while [ $i -lt 21 ]; do
       echo "Executing problem $i"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-$i-10.pddl $i-bc-10.soln.pp input/merging/port/best-cost/$i/$i-10.soln.pp 1800"
 	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-$i-20.pddl $i-bc-20.soln.pp input/merging/port/best-cost/$i/$i-20.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-$i-30.pddl $i-bc-30.soln.pp input/merging/port/best-cost/$i/$i-30.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-$i-40.pddl $i-bc-40.soln.pp input/merging/port/best-cost/$i/$i-40.soln.pp 1800"
      let i=i+5 
     done

echo "LOAD-BALANCE"
i=2
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-5.pddl $i-lb-05.soln.pp input/merging/port/load-balance/0$i/0$i-05.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-10.pddl $i-lb-10.soln.pp input/merging/port/load-balance/0$i/0$i-10.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-20.pddl $i-lb-20.soln.pp input/merging/port/load-balance/0$i/0$i-20.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-30.pddl $i-lb-30.soln.pp input/merging/port/load-balance/0$i/0$i-30.soln.pp 1800"
		
i=5
     while [ $i -lt 10 ]; do
       echo "Executing problem $i"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-5.pddl $i-lb-05.soln.pp input/merging/port/load-balance/0$i/0$i-05.soln.pp 1800" 
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-10.pddl $i-lb-10.soln.pp input/merging/port/load-balance/0$i/0$i-10.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-20.pddl $i-lb-20.soln.pp input/merging/port/load-balance/0$i/0$i-20.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-30.pddl $i-lb-30.soln.pp input/merging/port/load-balance/0$i/0$i-30.soln.pp 1800"
			let i=i+5 
     done
i=10
     while [ $i -lt 21 ]; do
       echo "Executing problem $i"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-$i-10.pddl $i-lb-10.soln.pp input/merging/port/load-balance/$i/$i-10.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-$i-20.pddl $i-lb-20.soln.pp input/merging/port/load-balance/$i/$i-20.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-$i-30.pddl $i-lb-30.soln.pp input/merging/port/load-balance/$i/$i-30.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-$i-40.pddl $i-lb-40.soln.pp input/merging/port/load-balance/$i/$i-40.soln.pp 1800" 
      let i=i+5 
     done


echo "REST-ACHIEVABLE"
i=2
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-5.pddl $i-ra-05.soln.pp input/merging/port/rest-achievable/0$i/0$i-05.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-10.pddl $i-ra-10.soln.pp input/merging/port/rest-achievable/0$i/0$i-10.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-20.pddl $i-ra-20.soln.pp input/merging/port/rest-achievable/0$i/0$i-20.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-30.pddl $i-ra-30.soln.pp input/merging/port/rest-achievable/0$i/0$i-30.soln.pp 1800"
		
i=5
     while [ $i -lt 10 ]; do
       echo "Executing problem $i"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-5.pddl $i-ra-05.soln.pp input/merging/port/rest-achievable/0$i/0$i-05.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-10.pddl $i-ra-10.soln.pp input/merging/port/rest-achievable/0$i/0$i-10.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-20.pddl $i-ra-20.soln.pp input/merging/port/rest-achievable/0$i/0$i-20.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-0$i-30.pddl $i-ra-30.soln.pp input/merging/port/rest-achievable/0$i/0$i-30.soln.pp 1800"
		 let i=i+5 
     done
i=10
     while [ $i -lt 21 ]; do
       echo "Executing problem $i"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-$i-10.pddl $i-ra-10.soln.pp input/merging/port/rest-achievable/$i/$i-10.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-$i-20.pddl $i-ra-20.soln.pp input/merging/port/rest-achievable/$i/$i-20.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-$i-30.pddl $i-ra-30.soln.pp input/merging/port/rest-achievable/$i/$i-30.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/domains/port/domain/domain.pddl ma-sat/domains/port/problems/pfile-$i-40.pddl $i-ra-40.soln.pp input/merging/port/rest-achievable/$i/$i-40.soln.pp 1800"
		 let i=i+5 
     done


i=1
echo "ALL-ACHIEVABLE"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/rover/domain/domain.pddl ma-sat/domains/rover/problems/ipc3-pfile0$i ro-0$i-ac.soln.pp input/merging/rover/all-achievable/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/rover/domain/domain.pddl ma-sat/domains/rover/problems/ipc3-pfile$i ro-$i-ac.soln.pp input/merging/rover/all-achievable/$i/$i.soln.pp 1800"
             let i=i+1 
         done

i=1
echo "BEST-COST"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/rover/domain/domain.pddl ma-sat/domains/rover/problems/ipc3-pfile0$i ro-0$i-bc.soln.pp input/merging/rover/best-cost/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/rover/domain/domain.pddl ma-sat/domains/rover/problems/ipc3-pfile$i ro-$i-bc.soln.pp input/merging/rover/best-cost/$i/$i.soln.pp 1800"
             let i=i+1 
         done

i=1
echo "LOAD-BALANCE"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/rover/domain/domain.pddl ma-sat/domains/rover/problems/ipc3-pfile0$i ro-0$i-lb.soln.pp input/merging/rover/load-balance/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/rover/domain/domain.pddl ma-sat/domains/rover/problems/ipc3-pfile$i ro-$i-lb.soln.pp input/merging/rover/load-balance/$i/$i.soln.pp 1800"
             let i=i+1 
         done

i=1
echo "REST-ACHIEVABLE"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/rover/domain/domain.pddl ma-sat/domains/rover/problems/ipc3-pfile0$i ro-0$i-ra.soln.pp input/merging/rover/rest-achievable/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/rover/domain/domain.pddl ma-sat/domains/rover/problems/ipc3-pfile$i ro-$i-ra.soln.pp input/merging/rover/rest-achievable/$i/$i.soln.pp 1800"
             let i=i+1 
         done

echo "UNIT-COST"

i=1
echo "ALL-ACHIEVABLE"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/rover/domain/domain.pddl ma-sat/domains/rover/problems/ipc3-pfile0$i ro-uc-0$i-ac.soln.pp input/unit-cost/rover/all-achievable/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/rover/domain/domain.pddl ma-sat/domains/rover/problems/ipc3-pfile$i ro-uc-$i-ac.soln.pp input/unit-cost/rover/all-achievable/$i/$i.soln.pp 1800"
             let i=i+1 
         done
         

i=1
echo "ALL-ACHIEVABLE"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/satellite/domain/domain.pddl ma-sat/domains/satellite/problems/pfile0$i sa-$i-ac.soln.pp input/merging/satellite/all-achievable/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/satellite/domain/domain.pddl ma-sat/domains/satellite/problems/pfile$i sa-$i-ac.soln.pp input/merging/satellite/all-achievable/$i/$i.soln.pp 1800"
             let i=i+1 
         done

i=1
echo "BEST-COST"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/satellite/domain/domain.pddl ma-sat/domains/satellite/problems/pfile0$i sa-$i-bc.soln.pp input/merging/satellite/best-cost/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/satellite/domain/domain.pddl ma-sat/domains/satellite/problems/pfile$i sa-$i-bc.soln.pp input/merging/satellite/best-cost/$i/$i.soln.pp 1800"
             let i=i+1 
         done

i=1
echo "LOAD-BALANCE"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/satellite/domain/domain.pddl ma-sat/domains/satellite/problems/pfile0$i sa-$i-lb.soln.pp input/merging/satellite/load-balance/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/satellite/domain/domain.pddl ma-sat/domains/satellite/problems/pfile$i sa-$i-lb.soln.pp input/merging/satellite/load-balance/$i/$i.soln.pp 1800"
             let i=i+1 
         done

i=1
echo "REST-ACHIEVABLE"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/satellite/domain/domain.pddl ma-sat/domains/satellite/problems/pfile0$i sa-$i-ra.soln.pp input/merging/satellite/rest-achievable/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/satellite/domain/domain.pddl ma-sat/domains/satellite/problems/pfile$i sa-$i-ra.soln.pp input/merging/satellite/rest-achievable/$i/$i.soln.pp 1800"
             let i=i+1 
         done

echo "UNIT COST"

i=1
echo "ALL-ACHIEVABLE"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/satellite/domain/domain.pddl ma-sat/domains/satellite/problems/pfile0$i sa-uc-$i-ac.soln.pp input/unit-cost/satellite/all-achievable/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/satellite/domain/domain.pddl ma-sat/domains/satellite/problems/pfile$i sa-uc-$i-ac.soln.pp input/unit-cost/satellite/all-achievable/$i/$i.soln.pp 1800"
             let i=i+1 
         done

i=1
echo "ALL-ACHIEVABLE"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/transport/domain/ma-domain.pddl ma-sat/domains/transport/problems/p0$i-ma.pddl tr-$i-ac.soln.pp input/merging/transport/all-achievable/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/transport/domain/ma-domain.pddl ma-sat/domains/transport/problems/p$i-ma.pddl tr-$i-ac.soln.pp input/merging/transport/all-achievable/$i/$i.soln.pp 1800"
             let i=i+1 
         done

i=1
echo "BEST-COST"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/transport/domain/ma-domain.pddl ma-sat/domains/transport/problems/p0$i-ma.pddl tr-$i-bc.soln.pp input/merging/transport/best-cost/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/transport/domain/ma-domain.pddl ma-sat/domains/transport/problems/p$i-ma.pddl tr-$i-bc.soln.pp input/merging/transport/best-cost/$i/$i.soln.pp 1800"
             let i=i+1 
         done

i=1
echo "LOAD-BALANCE"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/transport/domain/ma-domain.pddl ma-sat/domains/transport/problems/p0$i-ma.pddl tr-$i-lb.soln.pp input/merging/transport/load-balance/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/transport/domain/ma-domain.pddl ma-sat/domains/transport/problems/p$i-ma.pddl tr-$i-lb.soln.pp input/merging/transport/load-balance/$i/$i.soln.pp 1800"
             let i=i+1 
         done

i=1
echo "REST-ACHIEVABLE"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/transport/domain/ma-domain.pddl ma-sat/domains/transport/problems/p0$i-ma.pddl tr-$i-ra.soln.pp input/merging/transport/rest-achievable/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/transport/domain/ma-domain.pddl ma-sat/domains/transport/problems/p$i-ma.pddl tr-$i-ra.soln.pp input/merging/transport/rest-achievable/$i/$i.soln.pp 1800"
             let i=i+1 
         done

echo "UNIT-COST"
         
i=1
echo "ALL-ACHIEVABLE"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/transport/domain/ma-domain.pddl ma-sat/domains/transport/problems/p0$i-ma.pddl tr-uc-$i-ac.soln.pp input/unit-cost/transport/all-achievable/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/transport/domain/ma-domain.pddl ma-sat/domains/transport/problems/p$i-ma.pddl tr-uc-$i-ac.soln.pp input/unit-cost/transport/all-achievable/$i/$i.soln.pp 1800"
             let i=i+1 
         done

i=1
echo "ALL-ACHIEVABLE"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/zenotravel/domain/ma-zenotravel.pddl ma-sat/domains/zenotravel/problems/pfile0$i-ma ze-$i-ac.soln.pp input/merging/zenotravel/all-achievable/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/zenotravel/domain/ma-zenotravel.pddl ma-sat/domains/zenotravel/problems/pfile$i-ma ze-$i-ac.soln.pp input/merging/zenotravel/all-achievable/$i/$i.soln.pp 1800"
             let i=i+1 
         done


i=1
echo "BEST-COST"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/zenotravel/domain/ma-zenotravel.pddl ma-sat/domains/zenotravel/problems/pfile0$i-ma ze-$i-bc.soln.pp input/merging/zenotravel/best-cost/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/zenotravel/domain/ma-zenotravel.pddl ma-sat/domains/zenotravel/problems/pfile$i-ma ze-$i-bc.soln.pp input/merging/zenotravel/best-cost/$i/$i.soln.pp 1800"
             let i=i+1 
         done
         
i=1
echo "LOAD-BALANCE"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/zenotravel/domain/ma-zenotravel.pddl ma-sat/domains/zenotravel/problems/pfile0$i-ma ze-$i-lb.soln.pp input/merging/zenotravel/load-balance/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/zenotravel/domain/ma-zenotravel.pddl ma-sat/domains/zenotravel/problems/pfile$i-ma ze-$i-lb.soln.pp input/merging/zenotravel/load-balance/$i/$i.soln.pp 1800"
             let i=i+1 
         done

i=1
echo "REST-ACHIEVABLE"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/zenotravel/domain/ma-zenotravel.pddl ma-sat/domains/zenotravel/problems/pfile0$i-ma ze-$i-ra.soln.pp input/merging/zenotravel/rest-achievable/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/zenotravel/domain/ma-zenotravel.pddl ma-sat/domains/zenotravel/problems/pfile$i-ma ze-$i-ra.soln.pp input/merging/zenotravel/rest-achievable/$i/$i.soln.pp 1800"
             let i=i+1 
         done
         
echo "UNIT-COST"

i=1
echo "ALL-ACHIEVABLE"
         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/zenotravel/domain/ma-zenotravel.pddl ma-sat/domains/zenotravel/problems/pfile0$i-ma ze-uc-$i-ac.soln.pp input/unit-cost/zenotravel/all-achievable/0$i/0$i.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/domains/zenotravel/domain/ma-zenotravel.pddl ma-sat/domains/zenotravel/problems/pfile$i-ma ze-uc-$i-ac.soln.pp input/unit-cost/zenotravel/all-achievable/$i/$i.soln.pp 1800"
             let i=i+1 
         done


