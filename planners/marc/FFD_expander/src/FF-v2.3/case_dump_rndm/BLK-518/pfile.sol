# Domain file: zeno.pddl
# Problem file: zeno131_3.pddl
22
BOARD PERSON6 PLANE2 CITY9
FLY PLANE2 CITY9 CITY1 FL3 FL2
BOARD PERSON1 PLANE2 CITY1
FLY PLANE2 CITY1 CITY6 FL2 FL1
DEBARK PERSON1 PLANE2 CITY6
FLY PLANE1 CITY5 CITY10 FL4 FL3
BOARD PERSON4 PLANE1 CITY10
FLY PLANE1 CITY10 CITY5 FL3 FL2
DEBARK PERSON4 PLANE1 CITY5
FLY PLANE1 CITY5 CITY6 FL2 FL1
FLY PLANE2 CITY6 CITY3 FL1 FL0
REFUEL PLANE2 CITY3 FL0 FL1
BOARD PERSON2 PLANE2 CITY3
DEBARK PERSON6 PLANE2 CITY3
FLY PLANE2 CITY3 CITY8 FL1 FL0
REFUEL PLANE2 CITY8 FL0 FL1
BOARD PERSON3 PLANE2 CITY8
DEBARK PERSON2 PLANE2 CITY8
FLY PLANE2 CITY8 CITY9 FL1 FL0
REFUEL PLANE2 CITY9 FL0 FL1
DEBARK PERSON3 PLANE2 CITY9
FLY PLANE2 CITY9 CITY12 FL1 FL0
