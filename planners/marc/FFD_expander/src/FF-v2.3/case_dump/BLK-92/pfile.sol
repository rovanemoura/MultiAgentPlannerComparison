# Domain file: zeno.pddl
# Problem file: zeno127_3.pddl
11
BOARD PERSON2 PLANE4 CITY1
BOARD PERSON4 PLANE1 CITY2
FLY PLANE2 CITY1 CITY2 FL1 FL0
FLY PLANE4 CITY1 CITY2 FL1 FL0
REFUEL PLANE4 CITY2 FL0 FL1
DEBARK PERSON2 PLANE4 CITY2
FLY PLANE4 CITY2 CITY3 FL1 FL0
FLY PLANE1 CITY2 CITY3 FL1 FL0
REFUEL PLANE1 CITY3 FL0 FL1
DEBARK PERSON4 PLANE1 CITY3
FLY PLANE1 CITY3 CITY2 FL1 FL0