# Domain file: zeno.pddl
# Problem file: zeno249_1.pddl
19
REFUEL PLANE1 CITY6 FL0 FL1
BOARD PERSON4 PLANE1 CITY6
BOARD PERSON5 PLANE3 CITY1
FLY PLANE3 CITY1 CITY2 FL3 FL2
DEBARK PERSON5 PLANE3 CITY2
FLY PLANE3 CITY2 CITY4 FL2 FL1
BOARD PERSON3 PLANE3 CITY4
FLY PLANE3 CITY4 CITY2 FL1 FL0
DEBARK PERSON3 PLANE3 CITY2
FLY PLANE1 CITY6 CITY4 FL1 FL0
DEBARK PERSON4 PLANE1 CITY4
FLY PLANE2 CITY2 CITY5 FL2 FL1
BOARD PERSON2 PLANE2 CITY5
BOARD PERSON7 PLANE2 CITY5
FLY PLANE2 CITY5 CITY6 FL1 FL0
REFUEL PLANE2 CITY6 FL0 FL1
DEBARK PERSON2 PLANE2 CITY6
FLY PLANE2 CITY6 CITY1 FL1 FL0
DEBARK PERSON7 PLANE2 CITY1
