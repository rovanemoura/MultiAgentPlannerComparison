# Domain file: zeno.pddl
# Problem file: zeno131_2.pddl
25
REFUEL PLANE1 CITY3 FL0 FL1
FLY PLANE1 CITY3 CITY2 FL1 FL0
REFUEL PLANE1 CITY2 FL0 FL1
BOARD PERSON3 PLANE1 CITY2
FLY PLANE1 CITY2 CITY3 FL1 FL0
REFUEL PLANE1 CITY3 FL0 FL1
DEBARK PERSON3 PLANE1 CITY3
FLY PLANE1 CITY3 CITY6 FL1 FL0
REFUEL PLANE1 CITY6 FL0 FL1
BOARD PERSON4 PLANE1 CITY6
FLY PLANE1 CITY6 CITY5 FL1 FL0
REFUEL PLANE1 CITY5 FL0 FL1
DEBARK PERSON4 PLANE1 CITY5
FLY PLANE1 CITY5 CITY6 FL1 FL0
REFUEL PLANE1 CITY6 FL0 FL1
FLY PLANE1 CITY6 CITY7 FL1 FL0
FLY PLANE2 CITY8 CITY4 FL6 FL5
REFUEL PLANE1 CITY7 FL0 FL1
BOARD PERSON1 PLANE2 CITY4
BOARD PERSON2 PLANE2 CITY4
FLY PLANE2 CITY4 CITY6 FL5 FL4
DEBARK PERSON1 PLANE2 CITY6
FLY PLANE2 CITY6 CITY7 FL4 FL3
DEBARK PERSON2 PLANE2 CITY7
FLY PLANE1 CITY7 CITY4 FL1 FL0
