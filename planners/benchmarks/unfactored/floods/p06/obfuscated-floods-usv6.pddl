(define (domain floods)
  (:requirements :typing)
  (:types uav ugv usv - robot box cdm store disaster area robot - object)
  (:predicates (cdm_at ?c - cdm ?a - area) (box_at_cdm ?b - box ?c - cdm)
   (box_at_area ?b - box ?a - area) (water_path ?a1 - area ?a2 - area)
   (ground_path ?a1 - area ?a2 - area) (visible_from ?d - disaster ?a - area)
   (empty ?s - store) (full ?s - store)
   (have_water_sample_cdm ?c - cdm ?a - area) (communicated_data ?d - disaster)
   (in_range ?a1 - area ?a2 - area) (anon-at-usv6 ?a - area)
   (anon-store_of-usv6 ?s - store) (anon-have_picture-usv6 ?d - disaster)
   (anon-box_at-usv6 ?b - box ?r - ugv) (anon-have_water_sample-usv6 ?a - area))

  (:action anon-navigate_usv-usv6 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-usv6 ?a1) (water_path ?a1 ?a2)) :effect
   (and (not (anon-at-usv6 ?a1)) (anon-at-usv6 ?a2)))

  (:action anon-take_picture-usv6 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-usv6 ?a)) :effect
   (and (anon-have_picture-usv6 ?d)))

  (:action anon-sample_water-usv6 :parameters (?s - store ?a - area)
   :precondition (and (anon-at-usv6 ?a) (anon-store_of-usv6 ?s) (empty ?s))
   :effect (and (not (empty ?s)) (full ?s) (anon-have_water_sample-usv6 ?a)))

  (:action anon-drop_sample-usv6 :parameters
   (?s - store ?a1 - area ?a2 - area ?c - cdm) :precondition
   (and (anon-store_of-usv6 ?s) (anon-have_water_sample-usv6 ?a2)
        (cdm_at ?c ?a1) (anon-at-usv6 ?a1))
   :effect
   (and (not (full ?s)) (not (anon-have_water_sample-usv6 ?a2)) (empty ?s)
        (have_water_sample_cdm ?c ?a2)))

  (:action anon-communicate_data-usv6 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-usv6 ?a1) (cdm_at ?c ?a2) (anon-have_picture-usv6 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-usv6 ?d)) (communicated_data ?d)))
)