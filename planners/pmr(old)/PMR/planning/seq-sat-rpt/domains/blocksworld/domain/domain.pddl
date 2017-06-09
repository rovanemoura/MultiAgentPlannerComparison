(define (domain ma-blocksworld)
  (:requirements :strips)
  (:types block robot - object)
  (:predicates (clear ?x - block)
	       (ontable ?x - block)
	       (handempty ?x - robot)
	       (holding ?x - block ?r - robot)
	       (on ?x - block ?y - block))

  (:action pickup
   :parameters (?ob - block ?r - robot)
   :precondition (and (clear ?ob) (ontable ?ob) (handempty ?r))
   :effect (and (holding ?ob ?r) (not (clear ?ob)) (not (ontable ?ob)) 
		(not (handempty ?r))))

  (:action putdown
   :parameters  (?ob - block ?r - robot)
   :precondition (holding ?ob ?r)
   :effect (and (clear ?ob) (handempty ?r) (ontable ?ob) 
		(not (holding ?ob ?r))))

  (:action stack
   :parameters  (?ob - block ?underob - block ?r - robot)
   :precondition (and (clear ?underob) (holding ?ob ?r))
   :effect (and (handempty ?r) (clear ?ob) (on ?ob ?underob)
		(not (clear ?underob)) (not (holding ?ob ?r))))

  (:action unstack
   :parameters  (?ob - block ?underob - block ?r - robot)
   :precondition (and (on ?ob ?underob) (clear ?ob) (handempty ?r))
   :effect (and (holding ?ob ?r) (clear ?underob)
		(not (on ?ob ?underob)) (not (clear ?ob)) (not (handempty ?r)))))
