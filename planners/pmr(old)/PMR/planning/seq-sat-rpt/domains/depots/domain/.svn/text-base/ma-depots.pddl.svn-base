(define (domain depots)
(:requirements :typing)
(:types place locatable - object
	depot distributor - place
        truck hoist surface - locatable
        pallet crate - surface)

(:predicates (at-truck ?x - truck ?y - place) 
             (at-hoist ?x - hoist ?y - place) 
             (at ?x - locatable ?y - place) 
             (on ?x - crate ?y - surface)
             (in ?x - crate ?y - truck)
             (lifting ?x - hoist ?y - crate)
             (available ?x - hoist)
             (clear ?x - surface))
	
(:action Drive
:parameters (?x - truck ?y - place ?z - place) 
:precondition (and (at-truck ?x ?y))
:effect (and (not (at-truck ?x ?y)) (at-truck ?x ?z)))

(:action Lift
:parameters (?x - hoist ?y - crate ?z - surface ?p - place)
:precondition (and (at-hoist ?x ?p) (available ?x) (at ?y ?p) (on ?y ?z) (clear ?y))
:effect (and (not (at ?y ?p)) (lifting ?x ?y) (not (clear ?y)) (not (available ?x)) 
             (clear ?z) (not (on ?y ?z))))

(:action Drop 
:parameters (?x - hoist ?y - crate ?z - surface ?p - place)
:precondition (and (at-hoist ?x ?p) (at ?z ?p) (clear ?z) (lifting ?x ?y))
:effect (and (available ?x) (not (lifting ?x ?y)) (at ?y ?p) (not (clear ?z)) (clear ?y)
		(on ?y ?z)))

(:action Load
:parameters (?x - hoist ?y - crate ?z - truck ?p - place)
:precondition (and (at-hoist ?x ?p) (at-truck ?z ?p) (lifting ?x ?y))
:effect (and (not (lifting ?x ?y)) (in ?y ?z) (available ?x)))

(:action Unload 
:parameters (?x - hoist ?y - crate ?z - truck ?p - place)
:precondition (and (at-hoist ?x ?p) (at-truck ?z ?p) (available ?x) (in ?y ?z))
:effect (and (not (in ?y ?z)) (not (available ?x)) (lifting ?x ?y)))

)
