# Domain file: zeno.pddl
# Problem file: zeno136_3.pddl
12
REFUEL PLANE2 CITY3 FL0 FL1
FLY PLANE1 CITY3 CITY2 FL2 FL1
FLY PLANE4 CITY2 CITY1 FL5 FL4
BOARD PERSON1 PLANE4 CITY1
BOARD PERSON2 PLANE4 CITY1
BOARD PERSON3 PLANE4 CITY1
FLY PLANE4 CITY1 CITY2 FL4 FL3
DEBARK PERSON1 PLANE4 CITY2
DEBARK PERSON2 PLANE4 CITY2
FLY PLANE4 CITY2 CITY3 FL3 FL2
DEBARK PERSON3 PLANE4 CITY3
FLY PLANE2 CITY3 CITY1 FL1 FL0
