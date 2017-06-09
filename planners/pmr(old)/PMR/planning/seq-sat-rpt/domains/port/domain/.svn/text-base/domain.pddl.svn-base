(define (domain port)

    (:requirements :typing)

  (:types place hoist surface level - object
	  ship dock - place
	  pallet crate - surface
	  pallet-ship pallet-dock - pallet)

  ;; there will only be one dock for now
  
  (:predicates (at ?x - surface ?y - place) 
	       (on-ship ?x - crate ?y - surface ?s - ship)
	       (on-dock ?x - crate ?y - surface)
	       (lifting ?x - hoist ?y - crate)
	       (assigned ?x - hoist ?y - ship)
	       (available ?x - hoist)
	       (height ?s - surface ?l - level)
	       (next ?l - level ?l1 - level)
	       (clear ?x - surface))
	
  (:action lift-from-ship
	   :parameters (?h - hoist ?c - crate ?c1 - surface ?s - ship ?l - level)
	   :precondition (and (available ?h) (assigned ?h ?s) (at ?c ?s) (on-ship ?c ?c1 ?s) (clear ?c) (height ?c ?l))
	   :effect (and (not (at ?c ?s)) (lifting ?h ?c) (not (clear ?c)) (not (available ?h)) (clear ?c1) (not (height ?c ?l)) (not (on-ship ?c ?c1 ?s))))

  (:action lift-from-dock
	   :parameters (?h - hoist ?c - crate ?c1 - surface ?d - dock ?l - level)
	   :precondition (and (available ?h) (at ?c ?d) (on-dock ?c ?c1) (clear ?c) (height ?c ?l))
	   :effect (and (not (at ?c ?d)) (lifting ?h ?c) (not (clear ?c)) (not (available ?h)) (clear ?c1) (not (height ?c ?l)) (not (on-dock ?c ?c1))))

  (:action drop-in-ship
	   :parameters (?h - hoist ?c - crate ?c1 - surface ?s - ship ?l - level ?l1 - level)
	   :precondition (and (assigned ?h ?s) (at ?c1 ?s) (clear ?c1) (lifting ?h ?c) (height ?c1 ?l) (next ?l ?l1))
	   :effect (and (available ?h) (not (lifting ?h ?c)) (at ?c ?s) (not (clear ?c1)) (clear ?c) (on-ship ?c ?c1 ?s) (height ?c ?l1)))

  (:action drop-in-dock
	   :parameters (?h - hoist ?c - crate ?c1 - surface ?d - dock ?l - level ?l1 - level)
	   :precondition (and (at ?c1 ?d) (clear ?c1) (lifting ?h ?c) (height ?c1 ?l) (next ?l ?l1))
	   :effect (and (available ?h) (not (lifting ?h ?c)) (at ?c ?d) (not (clear ?c1)) (clear ?c) (on-dock ?c ?c1) (height ?c ?l1)))

  )
