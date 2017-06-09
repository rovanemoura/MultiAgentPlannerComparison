#!/bin/bash
echo "LPG"


i=2
       echo "Executing problem $i"
	   eval "./run-lpg-adapt.sh ma-sat/port/domain/domain.pddl ma-sat/port/problems/pfile-0$i-5.pddl $i-ac-05.soln.pp 1800"
 	   eval "./run-lpg-adapt.sh ma-sat/port/domain/domain.pddl ma-sat/port/problems/pfile-0$i-10.pddl $i-ac-10.soln.pp 1800"    
 	   eval "./run-lpg-adapt.sh ma-sat/port/domain/domain.pddl ma-sat/port/problems/pfile-0$i-20.pddl $i-ac-20.soln.pp 1800"   
 	   eval "./run-lpg-adapt.sh ma-sat/port/domain/domain.pddl ma-sat/port/problems/pfile-0$i-30.pddl $i-ac-30.soln.pp 1800"    		 


i=5
     while [ $i -lt 10 ]; do
       echo "Executing problem $i"
	   eval "./run-lpg-adapt.sh ma-sat/port/domain/domain.pddl ma-sat/port/problems/pfile-0$i-5.pddl $i-ac-05.soln.pp 1800"
 	   eval "./run-lpg-adapt.sh ma-sat/port/domain/domain.pddl ma-sat/port/problems/pfile-0$i-10.pddl $i-ac-10.soln.pp 1800"    
 	   eval "./run-lpg-adapt.sh ma-sat/port/domain/domain.pddl ma-sat/port/problems/pfile-0$i-20.pddl $i-ac-20.soln.pp 1800"   
 	   eval "./run-lpg-adapt.sh ma-sat/port/domain/domain.pddl ma-sat/port/problems/pfile-0$i-30.pddl $i-ac-30.soln.pp 1800"    		 
		 let i=i+5 
     done
i=10
     while [ $i -lt 21 ]; do
       echo "Executing problem $i"
	   eval "./run-lpg-adapt.sh ma-sat/port/domain/domain.pddl ma-sat/port/problems/pfile-$i-10.pddl $i-ac-10.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/port/domain/domain.pddl ma-sat/port/problems/pfile-$i-20.pddl $i-ac-20.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/port/domain/domain.pddl ma-sat/port/problems/pfile-$i-30.pddl $i-ac-30.soln.pp 1800"
	   eval "./run-lpg-adapt.sh ma-sat/port/domain/domain.pddl ma-sat/port/problems/pfile-$i-40.pddl $i-ac-40.soln.pp 1800"
		 let i=i+5 
     done
	 
i=1

         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/rover/domain/domain.pddl ma-sat/rover/problems/ipc3-pfile0$i ro-0$i-ac.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/rover/domain/domain.pddl ma-sat/rover/problems/ipc3-pfile$i ro-$i-ac.soln.pp 1800"
             let i=i+1 
         done


i=1

         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/satellite/domain/domain.pddl ma-sat/satellite/problems/pfile0$i sa-$i-ac.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/satellite/domain/domain.pddl ma-sat/satellite/problems/pfile$i sa-$i-ac.soln.pp 1800"
             let i=i+1 
         done


	 
i=1

         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/zenotravel/domain/ma-zenotravel.pddl ma-sat/zenotravel/problems/pfile0$i-ma ze-$i-ac.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/zenotravel/domain/ma-zenotravel.pddl ma-sat/zenotravel/problems/pfile$i-ma ze-$i-ac.soln.pp 1800"
             let i=i+1 
         done
		 
i=1

         while [  $i -lt 10 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/transport/domain/ma-domain.pddl ma-sat/transport/problems/p0$i-ma.pddl tr-$i-ac.soln.pp 1800"
             let i=i+1 
         done
i=10
         while [  $i -lt 21 ]; do
             echo "Executing problem $i"
	     eval "./run-lpg-adapt.sh ma-sat/transport/domain/ma-domain.pddl ma-sat/transport/problems/p$i-ma.pddl tr-$i-ac.soln.pp 1800"
             let i=i+1 
         done