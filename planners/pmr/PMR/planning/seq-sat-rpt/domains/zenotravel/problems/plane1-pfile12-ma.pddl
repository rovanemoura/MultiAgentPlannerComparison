(define (problem pfile12-ma) (:domain zenotravel)
  (:objects fl6 fl5 fl4 fl3 fl2 fl1 fl0 - flevel
             city5 city4 city3 city2 city1 city0 - city
             person8 person7 person6 person5 person4 person3 person2 person1 - person
             plane1 - aircraft
            )
  (:init
       (at-airplane plane1 city2)
       (fuel-level plane1 fl3)
       (at person1 city4)
       (at person2 city4)
       (at person3 city0)
       (at person4 city4)
       (at person5 city1)
       (at person6 city2)
       (at person7 city5)
       (at person8 city5)
       (next fl0 fl1)
       (next fl1 fl2)
       (next fl2 fl3)
       (next fl3 fl4)
       (next fl4 fl5)
       (next fl5 fl6))
  (:goal (and 
       (at person8 city4)
       (at person7 city3)
       (at person6 city1)
       (at person5 city4)
       (at person4 city4)
       (at person3 city1)
       (at person2 city1)
       (at person1 city2))))