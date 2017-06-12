(define (problem tiny_problem) (:domain zeno-travel)
(:objects
	person3 - person
	fl2 - flevel
	city21 - city
	fl0 - flevel
	city12 - city
	person16 - person
	fl1 - flevel
	person13 - person
	plane5 - aircraft
	city20 - city
	fl5 - flevel
	person4 - person
	city22 - city
	person18 - person
	fl6 - flevel
	person10 - person
	city3 - city
	person29 - person
	city17 - city
	person12 - person
	person11 - person
	city5 - city
	city23 - city
	person15 - person
	plane1 - aircraft
	person5 - person
	city8 - city
	plane4 - aircraft
	city9 - city
	fl4 - flevel
	person19 - person
	city4 - city
	person21 - person
	person8 - person
	person27 - person
	city13 - city
	person6 - person
	person1 - person
	city24 - city
	city6 - city
	person25 - person
	person7 - person
	fl3 - flevel
	city15 - city
	person14 - person
	city16 - city
	person30 - person
	person24 - person
	person23 - person
	plane2 - aircraft
	plane6 - aircraft
	city11 - city
	city19 - city
	city25 - city
	city10 - city
	person22 - person
	city18 - city
	person28 - person
	person2 - person
	city2 - city
	city7 - city
	person9 - person
	plane3 - aircraft
	city14 - city
	city1 - city
	person17 - person
	city0 - city
	person20 - person
	person26 - person
)(:init
	(at person15 city2)
	(at person4 city22)
	(next fl3 fl4)
	(next fl4 fl5)
	(at person29 city20)
	(at person16 city21)
	(next fl0 fl1)
	(at person9 city7)
	(at person10 city7)
	(at person11 city8)
	(next fl1 fl2)
	(at person27 city10)
	(next fl2 fl3)
	(at person18 city4)
	(at person21 city5)
	(at person3 city22)
	(next fl5 fl6)
	(at plane2 city18)
	(fuel-level plane2 fl0)
	(at person24 city18)
	(at person13 city0)
	(at person14 city1)
	(at plane1 city0)
	(fuel-level plane1 fl0)
	(at person2 city0)
	(at person7 city12)
	(at person12 city10)
	(at person28 city16)
	(at plane5 city1)
	(fuel-level plane5 fl0)
	(at person20 city1)
	(at plane4 city2)
	(fuel-level plane4 fl0)
	(at person22 city2)
	(at person19 city16)
	(at person30 city8)
	(at person5 city8)
	(at person17 city4)
	(at plane6 city4)
	(fuel-level plane6 fl0)
	(at person23 city4)
	(at person6 city4)
	(at person8 city16)
	(at person26 city12)
	(at plane3 city5)
	(fuel-level plane3 fl0)
	(at person1 city5)
	(in person25 plane3)
)(:goal (and 
	(at person1 city5)
	(at person12 city10)
	(at person13 city0)
	(at person14 city1)
	(at person17 city4)
	(at person19 city16)
	(at person2 city0)
	(at person20 city1)
	(at person22 city2)
	(at person23 city4)
	(at person24 city18)
	(at person25 city21)
	(at person26 city12)
	(at person28 city16)
	(at person30 city8)
	(at person5 city8)
	(at person6 city4)
	(at person7 city12)
	(at person8 city16)
)))