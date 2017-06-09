;; Transport sequential
;;

(define (domain transport)
  (:requirements :typing :action-costs)
  (:types
        location target locatable - object
        vehicle package - locatable
        capacity-number - object
  )

  (:predicates 
     (road ?l1 ?l2 - location)
     (at-truck ?x - vehicle ?v - location)
     (at ?x - package ?v - location)
     (in ?x - package ?v - vehicle)
     (capacity ?v - vehicle ?s1 - capacity-number)
     (capacity-predecessor ?s1 - capacity-number ?s2 - capacity-number)
  )

  (:functions
     (road-length ?l1 - location ?l2 - location)
     (total-cost)
  )

  (:action drive
    :parameters (?v - vehicle ?l1 - location ?l2 - location)
    :precondition (and
        (at-truck ?v ?l1)
        (road ?l1 ?l2)
      )
    :effect (and
        (not (at-truck ?v ?l1))
        (at-truck ?v ?l2)
        (increase (total-cost) (road-length ?l1 ?l2))
      )
  )

 (:action pick-up
    :parameters (?v - vehicle ?l - location ?p - package ?s1  - capacity-number ?s2 - capacity-number)
    :precondition (and
        (at-truck ?v ?l)
        (at ?p ?l)
        (capacity-predecessor ?s1 ?s2)
        (capacity ?v ?s2)
      )
    :effect (and
        (not (at ?p ?l))
        (in ?p ?v)
        (capacity ?v ?s1)
        (not (capacity ?v ?s2))
        (increase (total-cost) 1)
      )
  )

  (:action drop
    :parameters (?v - vehicle ?l - location ?p - package ?s1 - capacity-number ?s2 - capacity-number)
    :precondition (and
        (at-truck ?v ?l)
        (in ?p ?v)
        (capacity-predecessor ?s1 ?s2)
        (capacity ?v ?s1)
      )
    :effect (and
        (not (in ?p ?v))
        (at ?p ?l)
        (capacity ?v ?s2)
        (not (capacity ?v ?s1))
        (increase (total-cost) 1)
      )
  )

)
