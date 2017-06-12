(define (problem tiny_problem) (:domain zeno-travel)
(:objects
	city0 - city
	city4 - city
	fl3 - flevel
	plane1 - aircraft
	person7 - person
	person4 - person
	city1 - city
	plane3 - aircraft
	fl5 - flevel
	fl2 - flevel
	person3 - person
	city2 - city
	person5 - person
	person6 - person
	fl0 - flevel
	plane2 - aircraft
	person2 - person
	fl6 - flevel
	fl4 - flevel
	city3 - city
	fl1 - flevel
	person1 - person
)(:init
	(next fl0 fl1)
	(next fl5 fl6)
	(at person1 city4)
	(next fl1 fl2)
	(next fl3 fl4)
	(next fl4 fl5)
	(at person6 city3)
	(next fl2 fl3)
	(at plane2 city2)
	(fuel-level plane2 fl2)
	(at person2 city0)
	(at plane3 city3)
	(fuel-level plane3 fl0)
	(at person4 city3)
	(in person7 plane3)
	(at person3 city4)
	(at plane1 city1)
	(fuel-level plane1 fl1)
	(at person5 city1)
)(:goal (and 
	(at person2 city0)
	(at person3 city4)
	(at person4 city3)
	(at person5 city1)
	(at person6 city4)
)))