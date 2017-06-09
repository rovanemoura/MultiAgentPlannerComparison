(define (problem pfile09-ma) (:domain zenotravel)
  (:objects fl6 fl5 fl4 fl3 fl2 fl1 fl0 - flevel
             city4 city3 city2 city1 city0 - city
             person7 person6 person5 person4 person3 person2 person1 - person
             plane1 - aircraft
            )
  (:init
       (at-airplane plane1 city2)
       (fuel-level plane1 fl5)
       (at person1 city4)
       (at person2 city1)
       (at person3 city2)
       (at person4 city0)
       (at person5 city4)
       (at person6 city3)
       (at person7 city3)
       (next fl0 fl1)
       (next fl1 fl2)
       (next fl2 fl3)
       (next fl3 fl4)
       (next fl4 fl5)
       (next fl5 fl6))
  (:goal (and 
       (at person7 city4)
       (at person6 city4)
       (at person5 city1)
       (at person4 city3)
       (at person3 city4)
       (at person2 city0)
       (at person1 city2))))