(define (domain zenotravel)
(:requirements :typing)
(:types locatable city flevel - object
	aircraft person - locatable)
(:predicates (at ?x - person ?c - city)
             (at-airplane ?x - aircraft ?c - city)
             (in ?p - person ?a - aircraft)
	     (fuel-level ?a - aircraft ?l - flevel)
	     (next ?l1 - flevel ?l2 - flevel))


(:action board
 :parameters (?p - person ?a - aircraft ?c - city)
 
 :precondition (and (at ?p ?c)
                 (at-airplane ?a ?c))
 :effect (and (not (at ?p ?c))
              (in ?p ?a)))

(:action debark
 :parameters (?p - person ?a - aircraft ?c - city)

 :precondition (and (in ?p ?a)
                 (at-airplane ?a ?c))
 :effect (and (not (in ?p ?a))
              (at ?p ?c)))

(:action fly 
 :parameters (?a - aircraft ?c1 - city ?c2 - city ?l1 - flevel ?l2 - flevel)
 
 :precondition (and (at-airplane ?a ?c1)
                 (fuel-level ?a ?l1)
		 (next ?l2 ?l1))
 :effect (and (not (at-airplane ?a ?c1))
              (at-airplane ?a ?c2)
              (not (fuel-level ?a ?l1))
              (fuel-level ?a ?l2)))
                                  
(:action zoom
 :parameters (?a - aircraft ?c1 - city ?c2 - city ?l1 - flevel ?l2 - flevel ?l3 - flevel)

 :precondition (and (at-airplane ?a ?c1)
                 (fuel-level ?a ?l1)
		 (next ?l2 ?l1)
		 (next ?l3 ?l2)
		)
 :effect (and (not (at-airplane ?a ?c1))
              (at-airplane ?a ?c2)
              (not (fuel-level ?a ?l1))
              (fuel-level ?a ?l3)
	)
) 

(:action refuel
 :parameters (?a - aircraft ?c - city ?l - flevel ?l1 - flevel)

 :precondition (and (fuel-level ?a ?l)
                 (next ?l ?l1)
                 (at-airplane ?a ?c))
 :effect (and (fuel-level ?a ?l1) (not (fuel-level ?a ?l))))


)
