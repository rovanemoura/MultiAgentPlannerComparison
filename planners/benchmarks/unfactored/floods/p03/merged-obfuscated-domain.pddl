(define (domain floods)
  (:requirements :typing)
  (:types uav ugv usv - robot box cdm store disaster area robot - object)
  (:predicates (anon-have_water_sample-ugv3 ?r - usv ?a - area)
   (anon-box_at-ugv3 ?b - box) (anon-have_picture-ugv3 ?d - disaster)
   (anon-store_of-ugv3 ?s - store) (anon-at-ugv3 ?a - area)
   (in_range ?area - area ?area - area)
   (communicated_data ?disaster - disaster)
   (have_water_sample_cdm ?cdm - cdm ?area - area) (full ?store - store)
   (empty ?store - store) (visible_from ?disaster - disaster ?area - area)
   (ground_path ?area - area ?area - area)
   (water_path ?area - area ?area - area) (box_at_area ?box - box ?area - area)
   (box_at_cdm ?box - box ?cdm - cdm) (cdm_at ?cdm - cdm ?area - area)
   (anon-have_water_sample-uav4 ?r - usv ?a - area)
   (anon-box_at-uav4 ?b - box ?r - ugv) (anon-have_picture-uav4 ?d - disaster)
   (anon-store_of-uav4 ?s - store) (anon-at-uav4 ?a - area)
   (anon-have_water_sample-ugv1 ?r - usv ?a - area) (anon-box_at-ugv1 ?b - box)
   (anon-have_picture-ugv1 ?d - disaster) (anon-store_of-ugv1 ?s - store)
   (anon-at-ugv1 ?a - area) (anon-have_water_sample-usv4 ?a - area)
   (anon-box_at-usv4 ?b - box ?r - ugv) (anon-have_picture-usv4 ?d - disaster)
   (anon-store_of-usv4 ?s - store) (anon-at-usv4 ?a - area)
   (anon-at-usv2 ?a - area) (anon-store_of-usv2 ?s - store)
   (anon-have_picture-usv2 ?d - disaster) (anon-box_at-usv2 ?b - box ?r - ugv)
   (anon-have_water_sample-usv2 ?a - area))

  (:action anon-communicate_data-uav4 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-uav4 ?a1) (cdm_at ?c ?a2) (anon-have_picture-uav4 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-uav4 ?d)) (communicated_data ?d)))

  (:action anon-take_picture-uav4 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-uav4 ?a)) :effect
   (and (anon-have_picture-uav4 ?d)))

  (:action anon-navigate_uav-uav4 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-uav4 ?a1)) :effect
   (and (not (anon-at-uav4 ?a1)) (anon-at-uav4 ?a2)))

  (:action anon-communicate_data-usv4 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-usv4 ?a1) (cdm_at ?c ?a2) (anon-have_picture-usv4 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-usv4 ?d)) (communicated_data ?d)))

  (:action anon-drop_sample-usv4 :parameters
   (?s - store ?a1 - area ?a2 - area ?c - cdm) :precondition
   (and (anon-store_of-usv4 ?s) (anon-have_water_sample-usv4 ?a2)
        (cdm_at ?c ?a1) (anon-at-usv4 ?a1))
   :effect
   (and (not (full ?s)) (not (anon-have_water_sample-usv4 ?a2)) (empty ?s)
        (have_water_sample_cdm ?c ?a2)))

  (:action anon-sample_water-usv4 :parameters (?s - store ?a - area)
   :precondition (and (anon-at-usv4 ?a) (anon-store_of-usv4 ?s) (empty ?s))
   :effect (and (not (empty ?s)) (full ?s) (anon-have_water_sample-usv4 ?a)))

  (:action anon-take_picture-usv4 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-usv4 ?a)) :effect
   (and (anon-have_picture-usv4 ?d)))

  (:action anon-navigate_usv-usv4 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-usv4 ?a1) (water_path ?a1 ?a2)) :effect
   (and (not (anon-at-usv4 ?a1)) (anon-at-usv4 ?a2)))

  (:action anon-navigate_usv-usv2 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-usv2 ?a1) (water_path ?a1 ?a2)) :effect
   (and (not (anon-at-usv2 ?a1)) (anon-at-usv2 ?a2)))

  (:action anon-take_picture-usv2 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-usv2 ?a)) :effect
   (and (anon-have_picture-usv2 ?d)))

  (:action anon-sample_water-usv2 :parameters (?s - store ?a - area)
   :precondition (and (anon-at-usv2 ?a) (anon-store_of-usv2 ?s) (empty ?s))
   :effect (and (not (empty ?s)) (full ?s) (anon-have_water_sample-usv2 ?a)))

  (:action anon-drop_sample-usv2 :parameters
   (?s - store ?a1 - area ?a2 - area ?c - cdm) :precondition
   (and (anon-store_of-usv2 ?s) (anon-have_water_sample-usv2 ?a2)
        (cdm_at ?c ?a1) (anon-at-usv2 ?a1))
   :effect
   (and (not (full ?s)) (not (anon-have_water_sample-usv2 ?a2)) (empty ?s)
        (have_water_sample_cdm ?c ?a2)))

  (:action anon-communicate_data-usv2 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-usv2 ?a1) (cdm_at ?c ?a2) (anon-have_picture-usv2 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-usv2 ?d)) (communicated_data ?d)))

  (:action anon-navigate_ugv-ugv1 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-ugv1 ?a1) (ground_path ?a1 ?a2)) :effect
   (and (not (anon-at-ugv1 ?a1)) (anon-at-ugv1 ?a2)))

  (:action anon-take_picture-ugv1 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-ugv1 ?a)) :effect
   (and (anon-have_picture-ugv1 ?d)))

  (:action anon-pickup_box-ugv1 :parameters
   (?s - store ?c - cdm ?a - area ?b - box) :precondition
   (and (box_at_cdm ?b ?c) (cdm_at ?c ?a) (anon-at-ugv1 ?a)
        (anon-store_of-ugv1 ?s) (empty ?s))
   :effect
   (and (not (empty ?s)) (not (box_at_cdm ?b ?c)) (anon-box_at-ugv1 ?b)
        (full ?s)))

  (:action anon-drop_box-ugv1 :parameters (?s - store ?a - area ?b - box)
   :precondition
   (and (anon-box_at-ugv1 ?b) (anon-store_of-ugv1 ?s) (anon-at-ugv1 ?a))
   :effect
   (and (not (full ?s)) (not (anon-box_at-ugv1 ?b)) (box_at_area ?b ?a)
        (empty ?s)))

  (:action anon-communicate_data-ugv1 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-ugv1 ?a1) (cdm_at ?c ?a2) (anon-have_picture-ugv1 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-ugv1 ?d)) (communicated_data ?d)))

  (:action anon-navigate_ugv-ugv3 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-ugv3 ?a1) (ground_path ?a1 ?a2)) :effect
   (and (not (anon-at-ugv3 ?a1)) (anon-at-ugv3 ?a2)))

  (:action anon-take_picture-ugv3 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-ugv3 ?a)) :effect
   (and (anon-have_picture-ugv3 ?d)))

  (:action anon-pickup_box-ugv3 :parameters
   (?s - store ?c - cdm ?a - area ?b - box) :precondition
   (and (box_at_cdm ?b ?c) (cdm_at ?c ?a) (anon-at-ugv3 ?a)
        (anon-store_of-ugv3 ?s) (empty ?s))
   :effect
   (and (not (empty ?s)) (not (box_at_cdm ?b ?c)) (anon-box_at-ugv3 ?b)
        (full ?s)))

  (:action anon-drop_box-ugv3 :parameters (?s - store ?a - area ?b - box)
   :precondition
   (and (anon-box_at-ugv3 ?b) (anon-store_of-ugv3 ?s) (anon-at-ugv3 ?a))
   :effect
   (and (not (full ?s)) (not (anon-box_at-ugv3 ?b)) (box_at_area ?b ?a)
        (empty ?s)))

  (:action anon-communicate_data-ugv3 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-ugv3 ?a1) (cdm_at ?c ?a2) (anon-have_picture-ugv3 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-ugv3 ?d)) (communicated_data ?d)))
)