(define (problem pfile10-ma) (:domain zenotravel)
  (:objects fl6 fl5 fl4 fl3 fl2 fl1 fl0 - flevel
             city4 city3 city2 city1 city0 - city
             person8 person7 person6 person5 person4 person3 person2 person1 - person
             plane3 - aircraft
            )
  (:init
       (at-airplane plane3 city2)
       (fuel-level plane3 fl2)
       (at person1 city3)
       (at person2 city3)
       (at person3 city4)
       (at person4 city4)
       (at person5 city1)
       (at person6 city0)
       (at person7 city1)
       (at person8 city0)
       (next fl0 fl1)
       (next fl1 fl2)
       (next fl2 fl3)
       (next fl3 fl4)
       (next fl4 fl5)
       (next fl5 fl6))
  (:goal (and 
       (at person8 city3)
       (at person7 city4)
       (at person6 city3)
       (at person5 city0)
       (at person4 city1)
       (at person3 city3)
       (at person2 city2)
       (at person1 city1))))