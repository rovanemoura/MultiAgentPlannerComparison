(define (domain floods)
  (:requirements :typing)
  (:types uav ugv usv - robot box cdm store disaster area robot - object)
  (:predicates (cdm_at ?c - cdm ?a - area) (box_at_cdm ?b - box ?c - cdm)
   (box_at_area ?b - box ?a - area) (water_path ?a1 - area ?a2 - area)
   (ground_path ?a1 - area ?a2 - area) (visible_from ?d - disaster ?a - area)
   (empty ?s - store) (full ?s - store)
   (have_water_sample_cdm ?c - cdm ?a - area) (communicated_data ?d - disaster)
   (in_range ?a1 - area ?a2 - area) (anon-at-uav7 ?a - area)
   (anon-store_of-uav7 ?s - store) (anon-have_picture-uav7 ?d - disaster)
   (anon-box_at-uav7 ?b - box ?r - ugv)
   (anon-have_water_sample-uav7 ?r - usv ?a - area))

  (:action anon-navigate_uav-uav7 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-uav7 ?a1)) :effect
   (and (not (anon-at-uav7 ?a1)) (anon-at-uav7 ?a2)))

  (:action anon-take_picture-uav7 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-uav7 ?a)) :effect
   (and (anon-have_picture-uav7 ?d)))

  (:action anon-communicate_data-uav7 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-uav7 ?a1) (cdm_at ?c ?a2) (anon-have_picture-uav7 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-uav7 ?d)) (communicated_data ?d)))
)