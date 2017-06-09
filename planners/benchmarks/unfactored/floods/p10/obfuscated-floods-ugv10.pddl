(define (domain floods)
  (:requirements :typing)
  (:types uav ugv usv - robot box cdm store disaster area robot - object)
  (:predicates (cdm_at ?c - cdm ?a - area) (box_at_cdm ?b - box ?c - cdm)
   (box_at_area ?b - box ?a - area) (water_path ?a1 - area ?a2 - area)
   (ground_path ?a1 - area ?a2 - area) (visible_from ?d - disaster ?a - area)
   (empty ?s - store) (full ?s - store)
   (have_water_sample_cdm ?c - cdm ?a - area) (communicated_data ?d - disaster)
   (in_range ?a1 - area ?a2 - area) (anon-at-ugv10 ?a - area)
   (anon-store_of-ugv10 ?s - store) (anon-have_picture-ugv10 ?d - disaster)
   (anon-box_at-ugv10 ?b - box)
   (anon-have_water_sample-ugv10 ?r - usv ?a - area))

  (:action anon-navigate_ugv-ugv10 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-ugv10 ?a1) (ground_path ?a1 ?a2)) :effect
   (and (not (anon-at-ugv10 ?a1)) (anon-at-ugv10 ?a2)))

  (:action anon-take_picture-ugv10 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-ugv10 ?a)) :effect
   (and (anon-have_picture-ugv10 ?d)))

  (:action anon-pickup_box-ugv10 :parameters
   (?s - store ?c - cdm ?a - area ?b - box) :precondition
   (and (box_at_cdm ?b ?c) (cdm_at ?c ?a) (anon-at-ugv10 ?a)
        (anon-store_of-ugv10 ?s) (empty ?s))
   :effect
   (and (not (empty ?s)) (not (box_at_cdm ?b ?c)) (full ?s)
        (anon-box_at-ugv10 ?b)))

  (:action anon-drop_box-ugv10 :parameters (?s - store ?a - area ?b - box)
   :precondition
   (and (anon-box_at-ugv10 ?b) (anon-store_of-ugv10 ?s) (anon-at-ugv10 ?a))
   :effect
   (and (not (full ?s)) (not (anon-box_at-ugv10 ?b)) (empty ?s)
        (box_at_area ?b ?a)))

  (:action anon-communicate_data-ugv10 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-ugv10 ?a1) (cdm_at ?c ?a2) (anon-have_picture-ugv10 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-ugv10 ?d)) (communicated_data ?d)))
)