(define (problem ZTRAVEL-33-55)
(:domain zenotravel)
(:objects
	plane1 - aircraft
	plane2 - aircraft
	plane3 - aircraft
	plane4 - aircraft
	plane5 - aircraft
	plane6 - aircraft
	plane7 - aircraft
	plane8 - aircraft
	plane9 - aircraft
	plane10 - aircraft
	plane11 - aircraft
	plane12 - aircraft
	plane13 - aircraft
	plane14 - aircraft
	plane15 - aircraft
	plane16 - aircraft
	plane17 - aircraft
	plane18 - aircraft
	plane19 - aircraft
	plane20 - aircraft
	plane21 - aircraft
	plane22 - aircraft
	plane23 - aircraft
	plane24 - aircraft
	plane25 - aircraft
	plane26 - aircraft
	plane27 - aircraft
	plane28 - aircraft
	plane29 - aircraft
	plane30 - aircraft
	plane31 - aircraft
	plane32 - aircraft
	plane33 - aircraft
	person1 - person
	person2 - person
	person3 - person
	person4 - person
	person5 - person
	person6 - person
	person7 - person
	person8 - person
	person9 - person
	person10 - person
	person11 - person
	person12 - person
	person13 - person
	person14 - person
	person15 - person
	person16 - person
	person17 - person
	person18 - person
	person19 - person
	person20 - person
	person21 - person
	person22 - person
	person23 - person
	person24 - person
	person25 - person
	person26 - person
	person27 - person
	person28 - person
	person29 - person
	person30 - person
	person31 - person
	person32 - person
	person33 - person
	person34 - person
	person35 - person
	person36 - person
	person37 - person
	person38 - person
	person39 - person
	person40 - person
	person41 - person
	person42 - person
	person43 - person
	person44 - person
	person45 - person
	person46 - person
	person47 - person
	person48 - person
	person49 - person
	person50 - person
	person51 - person
	person52 - person
	person53 - person
	person54 - person
	person55 - person
	city0 - city
	city1 - city
	city2 - city
	city3 - city
	city4 - city
	city5 - city
	city6 - city
	city7 - city
	city8 - city
	city9 - city
	city10 - city
	city11 - city
	city12 - city
	city13 - city
	city14 - city
	city15 - city
	city16 - city
	city17 - city
	city18 - city
	city19 - city
	city20 - city
	city21 - city
	fl0 - flevel
	fl1 - flevel
	fl2 - flevel
	fl3 - flevel
	fl4 - flevel
	fl5 - flevel
	fl6 - flevel
	)
(:init
	(at-airplane plane1 city3)
	(fuel-level plane1 fl2)
	(at-airplane plane2 city3)
	(fuel-level plane2 fl4)
	(at-airplane plane3 city18)
	(fuel-level plane3 fl0)
	(at-airplane plane4 city3)
	(fuel-level plane4 fl3)
	(at-airplane plane5 city14)
	(fuel-level plane5 fl0)
	(at-airplane plane6 city19)
	(fuel-level plane6 fl2)
	(at-airplane plane7 city2)
	(fuel-level plane7 fl1)
	(at-airplane plane8 city5)
	(fuel-level plane8 fl2)
	(at-airplane plane9 city8)
	(fuel-level plane9 fl5)
	(at-airplane plane10 city14)
	(fuel-level plane10 fl3)
	(at-airplane plane11 city17)
	(fuel-level plane11 fl5)
	(at-airplane plane12 city6)
	(fuel-level plane12 fl1)
	(at-airplane plane13 city1)
	(fuel-level plane13 fl0)
	(at-airplane plane14 city12)
	(fuel-level plane14 fl1)
	(at-airplane plane15 city11)
	(fuel-level plane15 fl4)
	(at-airplane plane16 city3)
	(fuel-level plane16 fl4)
	(at-airplane plane17 city3)
	(fuel-level plane17 fl3)
	(at-airplane plane18 city5)
	(fuel-level plane18 fl1)
	(at-airplane plane19 city17)
	(fuel-level plane19 fl6)
	(at-airplane plane20 city8)
	(fuel-level plane20 fl2)
	(at-airplane plane21 city10)
	(fuel-level plane21 fl5)
	(at-airplane plane22 city10)
	(fuel-level plane22 fl2)
	(at-airplane plane23 city7)
	(fuel-level plane23 fl6)
	(at-airplane plane24 city6)
	(fuel-level plane24 fl6)
	(at-airplane plane25 city13)
	(fuel-level plane25 fl6)
	(at-airplane plane26 city12)
	(fuel-level plane26 fl6)
	(at-airplane plane27 city7)
	(fuel-level plane27 fl1)
	(at-airplane plane28 city13)
	(fuel-level plane28 fl4)
	(at-airplane plane29 city21)
	(fuel-level plane29 fl0)
	(at-airplane plane30 city14)
	(fuel-level plane30 fl6)
	(at-airplane plane31 city3)
	(fuel-level plane31 fl5)
	(at-airplane plane32 city16)
	(fuel-level plane32 fl1)
	(at-airplane plane33 city7)
	(fuel-level plane33 fl2)
	(at person1 city14)
	(at person2 city10)
	(at person3 city19)
	(at person4 city13)
	(at person5 city5)
	(at person6 city15)
	(at person7 city17)
	(at person8 city15)
	(at person9 city5)
	(at person10 city1)
	(at person11 city3)
	(at person12 city4)
	(at person13 city10)
	(at person14 city18)
	(at person15 city6)
	(at person16 city0)
	(at person17 city7)
	(at person18 city0)
	(at person19 city5)
	(at person20 city7)
	(at person21 city11)
	(at person22 city18)
	(at person23 city11)
	(at person24 city18)
	(at person25 city4)
	(at person26 city20)
	(at person27 city12)
	(at person28 city9)
	(at person29 city10)
	(at person30 city20)
	(at person31 city10)
	(at person32 city17)
	(at person33 city8)
	(at person34 city2)
	(at person35 city13)
	(at person36 city1)
	(at person37 city5)
	(at person38 city20)
	(at person39 city17)
	(at person40 city3)
	(at person41 city1)
	(at person42 city14)
	(at person43 city4)
	(at person44 city2)
	(at person45 city4)
	(at person46 city18)
	(at person47 city20)
	(at person48 city20)
	(at person49 city16)
	(at person50 city16)
	(at person51 city4)
	(at person52 city14)
	(at person53 city20)
	(at person54 city12)
	(at person55 city9)
	(next fl0 fl1)
	(next fl1 fl2)
	(next fl2 fl3)
	(next fl3 fl4)
	(next fl4 fl5)
	(next fl5 fl6)
)
(:goal (and
	(at-airplane plane4 city19)
	(at-airplane plane12 city7)
	(at-airplane plane13 city16)
	(at-airplane plane16 city19)
	(at-airplane plane24 city9)
	(at-airplane plane26 city14)
	(at person1 city3)
	(at person2 city8)
	(at person3 city13)
	(at person4 city0)
	(at person5 city7)
	(at person6 city20)
	(at person7 city4)
	(at person8 city4)
	(at person9 city8)
	(at person10 city2)
	(at person11 city16)
	(at person12 city5)
	(at person13 city2)
	(at person14 city16)
	(at person15 city0)
	(at person16 city15)
	(at person17 city10)
	(at person18 city4)
	(at person19 city10)
	(at person20 city12)
	(at person21 city15)
	(at person22 city20)
	(at person23 city8)
	(at person24 city4)
	(at person25 city10)
	(at person26 city11)
	(at person27 city18)
	(at person28 city19)
	(at person29 city2)
	(at person30 city10)
	(at person31 city21)
	(at person32 city18)
	(at person33 city7)
	(at person34 city4)
	(at person35 city9)
	(at person36 city7)
	(at person37 city19)
	(at person38 city6)
	(at person39 city17)
	(at person40 city15)
	(at person41 city4)
	(at person42 city0)
	(at person43 city9)
	(at person44 city11)
	(at person45 city2)
	(at person46 city4)
	(at person47 city9)
	(at person48 city8)
	(at person49 city4)
	(at person50 city7)
	(at person51 city9)
	(at person52 city1)
	(at person53 city6)
	(at person54 city9)
	(at person55 city13)
	))

)