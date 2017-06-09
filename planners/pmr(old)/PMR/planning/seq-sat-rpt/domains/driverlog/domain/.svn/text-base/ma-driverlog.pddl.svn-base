(define (domain driverlog)
  (:requirements :typing) 
  (:types         location locatable - object
		driver truck obj - locatable)
  (:predicates 
		(at-truck ?tr - truck ?loc - location)
		(at ?obj - obj ?loc - location)
		(at-driver ?dri - driver ?loc - location)
		(in ?obj1 - obj ?obj - truck)
		(driving ?d - driver ?v - truck)
		(link ?x ?y - location) (path ?x ?y - location)
		(empty ?v - truck))


(:action LOAD-TRUCK
  :parameters  (?obj - obj  ?truck - truck  ?loc - location)
  :precondition  (and (at-truck ?truck ?loc) (at ?obj ?loc))
  :effect  (and (not (at ?obj ?loc)) (in ?obj ?truck)))

(:action UNLOAD-TRUCK
  :parameters (?obj - obj  ?truck - truck  ?loc - location)
  :precondition  (and (at-truck ?truck ?loc) (in ?obj ?truck))
  :effect (and (not (in ?obj ?truck)) (at ?obj ?loc)))

(:action BOARD-TRUCK
  :parameters (?drv - driver  ?truck - truck ?loc - location)
  :precondition (and (at-truck ?truck ?loc) (at-driver ?drv ?loc) (empty ?truck))
  :effect (and (not (at-driver ?drv ?loc)) (driving ?drv ?truck) 
	       (not (empty ?truck))))

(:action DISEMBARK-TRUCK
  :parameters  (?drv - driver  ?truck - truck  ?loc - location)
  :precondition  (and (at-truck ?truck ?loc) (driving ?drv ?truck))
  :effect  (and (not (driving ?drv ?truck)) (at-driver ?drv ?loc) 
	        (empty ?truck)))

(:action DRIVE-TRUCK
  :parameters (?truck - truck  ?loc-from - location  
	       ?loc-to - location  ?drv - driver)
  :precondition  (and (at-truck ?truck ?loc-from)  (driving ?drv ?truck) 
	              (link ?loc-from ?loc-to))
  :effect  (and (not (at-truck ?truck ?loc-from)) (at-truck ?truck ?loc-to)))

(:action WALK
  :parameters (?drv - driver  ?loc-from - location  
	       ?loc-to - location)
  :precondition  (and (at-driver ?drv ?loc-from) (path ?loc-from ?loc-to))
  :effect (and (not (at-driver ?drv ?loc-from)) (at-driver ?drv ?loc-to)))
)
