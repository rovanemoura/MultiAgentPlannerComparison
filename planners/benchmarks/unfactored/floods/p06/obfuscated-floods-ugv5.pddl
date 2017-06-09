(define (domain floods)
  (:requirements :typing)
  (:types uav ugv usv - robot box cdm store disaster area robot - object)
  (:predicates (cdm_at ?c - cdm ?a - area) (box_at_cdm ?b - box ?c - cdm)
   (box_at_area ?b - box ?a - area) (water_path ?a1 - area ?a2 - area)
   (ground_path ?a1 - area ?a2 - area) (visible_from ?d - disaster ?a - area)
   (empty ?s - store) (full ?s - store)
   (have_water_sample_cdm ?c - cdm ?a - area) (communicated_data ?d - disaster)
   (in_range ?a1 - area ?a2 - area) (anon-at-ugv5 ?a - area)
   (anon-store_of-ugv5 ?s - store) (anon-have_picture-ugv5 ?d - disaster)
   (anon-box_at-ugv5 ?b - box) (anon-have_water_sample-ugv5 ?r - usv ?a - area))

  (:action anon-navigate_ugv-ugv5 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-ugv5 ?a1) (ground_path ?a1 ?a2)) :effect
   (and (not (anon-at-ugv5 ?a1)) (anon-at-ugv5 ?a2)))

  (:action anon-take_picture-ugv5 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-ugv5 ?a)) :effect
   (and (anon-have_picture-ugv5 ?d)))

  (:action anon-pickup_box-ugv5 :parameters
   (?s - store ?c - cdm ?a - area ?b - box) :precondition
   (and (box_at_cdm ?b ?c) (cdm_at ?c ?a) (anon-at-ugv5 ?a)
        (anon-store_of-ugv5 ?s) (empty ?s))
   :effect
   (and (not (empty ?s)) (not (box_at_cdm ?b ?c)) (anon-box_at-ugv5 ?b)
        (full ?s)))

  (:action anon-drop_box-ugv5 :parameters (?s - store ?a - area ?b - box)
   :precondition
   (and (anon-box_at-ugv5 ?b) (anon-store_of-ugv5 ?s) (anon-at-ugv5 ?a))
   :effect
   (and (not (full ?s)) (not (anon-box_at-ugv5 ?b)) (box_at_area ?b ?a)
        (empty ?s)))

  (:action anon-communicate_data-ugv5 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-ugv5 ?a1) (cdm_at ?c ?a2) (anon-have_picture-ugv5 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-ugv5 ?d)) (communicated_data ?d)))
)