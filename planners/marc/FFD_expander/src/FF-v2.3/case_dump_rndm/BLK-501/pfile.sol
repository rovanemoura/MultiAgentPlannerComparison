# Domain file: zeno.pddl
# Problem file: zeno154_1.pddl
16
REFUEL PLANE2 CITY1 FL0 FL1
BOARD PERSON1 PLANE1 CITY2
BOARD PERSON2 PLANE2 CITY1
BOARD PERSON5 PLANE1 CITY2
FLY PLANE1 CITY2 CITY5 FL5 FL4
DEBARK PERSON1 PLANE1 CITY5
FLY PLANE1 CITY5 CITY3 FL4 FL3
DEBARK PERSON5 PLANE1 CITY3
FLY PLANE3 CITY3 CITY7 FL3 FL2
BOARD PERSON3 PLANE3 CITY7
FLY PLANE3 CITY7 CITY3 FL2 FL1
DEBARK PERSON3 PLANE3 CITY3
FLY PLANE2 CITY1 CITY7 FL1 FL0
REFUEL PLANE2 CITY7 FL0 FL1
DEBARK PERSON2 PLANE2 CITY7
FLY PLANE2 CITY7 CITY6 FL1 FL0