(define (problem ZTRAVEL-3-10) (:domain zeno-travel)
(:objects
	plane1 - aircraft
	fl2 - flevel
	fl3 - flevel
	fl0 - flevel
	fl1 - flevel
	fl6 - flevel
	city4 - city
	fl4 - flevel
	fl5 - flevel
	city1 - city
	city0 - city
	city3 - city
	city2 - city
	city5 - city
	person10 - person
	person9 - person
	person8 - person
	person1 - person
	person3 - person
	person2 - person
	person5 - person
	person4 - person
	person7 - person
	person6 - person
)
(:init
	(at plane1 city4)
	(fuel-level plane1 fl2)
	(at person1 city1)
	(at person2 city2)
	(at person3 city1)
	(at person4 city4)
	(at person5 city5)
	(at person6 city1)
	(at person7 city0)
	(at person8 city2)
	(at person9 city1)
	(at person10 city5)
	(next fl0 fl1)
	(next fl1 fl2)
	(next fl2 fl3)
	(next fl3 fl4)
	(next fl4 fl5)
	(next fl5 fl6)
)
(:goal
	(and
		(at plane1 city4)
		(at person1 city4)
		(at person2 city5)
		(at person3 city4)
		(at person4 city0)
		(at person5 city2)
		(at person6 city3)
		(at person8 city0)
		(at person9 city3)
		(at person10 city4)
	)
)
)
