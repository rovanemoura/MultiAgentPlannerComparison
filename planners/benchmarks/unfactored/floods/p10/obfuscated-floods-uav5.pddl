(define (domain floods)
  (:requirements :typing)
  (:types uav ugv usv - robot box cdm store disaster area robot - object)
  (:predicates (cdm_at ?c - cdm ?a - area) (box_at_cdm ?b - box ?c - cdm)
   (box_at_area ?b - box ?a - area) (water_path ?a1 - area ?a2 - area)
   (ground_path ?a1 - area ?a2 - area) (visible_from ?d - disaster ?a - area)
   (empty ?s - store) (full ?s - store)
   (have_water_sample_cdm ?c - cdm ?a - area) (communicated_data ?d - disaster)
   (in_range ?a1 - area ?a2 - area) (anon-at-uav5 ?a - area)
   (anon-store_of-uav5 ?s - store) (anon-have_picture-uav5 ?d - disaster)
   (anon-box_at-uav5 ?b - box ?r - ugv)
   (anon-have_water_sample-uav5 ?r - usv ?a - area))

  (:action anon-navigate_uav-uav5 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-uav5 ?a1)) :effect
   (and (not (anon-at-uav5 ?a1)) (anon-at-uav5 ?a2)))

  (:action anon-take_picture-uav5 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-uav5 ?a)) :effect
   (and (anon-have_picture-uav5 ?d)))

  (:action anon-communicate_data-uav5 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-uav5 ?a1) (cdm_at ?c ?a2) (anon-have_picture-uav5 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-uav5 ?d)) (communicated_data ?d)))
)