;; logistics domain multi-agent version

(define (domain logistics)
  (:requirements :strips :typing) 
  (:types truck airplane - vehicle
          airport location - place
          city package - object)
  
  (:predicates 	(in-city ?loc - place ?city - city)
		(at ?veh - vehicle ?loc - place)
		(inside ?pkg - package ?veh - vehicle)
		(at-pkg ?pkg - package ?loc - place))
  
(:action LOAD-TRUCK
   :parameters    (?pkg - package ?truck - truck ?loc - place)
   :precondition  (and (at ?truck ?loc) (at-pkg ?pkg ?loc))
   :effect        (and (not (at-pkg ?pkg ?loc)) (inside ?pkg ?truck)))

(:action LOAD-AIRPLANE
  :parameters   (?pkg - package ?airplane - airplane ?loc - place)
  :precondition (and (at-pkg ?pkg ?loc) (at ?airplane ?loc))
  :effect       (and (not (at-pkg ?pkg ?loc)) (inside ?pkg ?airplane)))

(:action UNLOAD-TRUCK
  :parameters   (?pkg - package ?truck - truck ?loc - place)
  :precondition (and (at ?truck ?loc) (inside ?pkg ?truck))
  :effect       (and (not (inside ?pkg ?truck)) (at-pkg ?pkg ?loc)))

(:action UNLOAD-AIRPLANE
  :parameters    (?pkg - package ?airplane - airplane ?loc - place)
  :precondition  (and (inside ?pkg ?airplane) (at ?airplane ?loc))
  :effect        (and (not (inside ?pkg ?airplane)) (at-pkg ?pkg ?loc)))

(:action DRIVE-TRUCK
  :parameters (?truck - truck ?loc-from - place ?loc-to - place ?city - city)
  :precondition
   (and (at ?truck ?loc-from) (in-city ?loc-from ?city) (in-city ?loc-to ?city))
  :effect
   (and (not (at ?truck ?loc-from)) (at ?truck ?loc-to)))

(:action FLY-AIRPLANE
  :parameters (?airplane - airplane ?loc-from - airport ?loc-to - airport)
  :precondition
   (at ?airplane ?loc-from)
  :effect
   (and (not (at ?airplane ?loc-from)) (at ?airplane ?loc-to)))
)
