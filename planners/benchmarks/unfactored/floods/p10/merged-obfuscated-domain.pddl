(define (domain floods)
  (:requirements :typing)
  (:types uav ugv usv - robot box cdm store disaster area robot - object)
  (:predicates (anon-have_water_sample-usv1 ?a - area)
   (anon-box_at-usv1 ?b - box ?r - ugv) (anon-have_picture-usv1 ?d - disaster)
   (anon-store_of-usv1 ?s - store) (anon-at-usv1 ?a - area)
   (in_range ?area - area ?area - area)
   (communicated_data ?disaster - disaster)
   (have_water_sample_cdm ?cdm - cdm ?area - area) (full ?store - store)
   (empty ?store - store) (visible_from ?disaster - disaster ?area - area)
   (ground_path ?area - area ?area - area)
   (water_path ?area - area ?area - area) (box_at_area ?box - box ?area - area)
   (box_at_cdm ?box - box ?cdm - cdm) (cdm_at ?cdm - cdm ?area - area)
   (anon-have_water_sample-uav11 ?r - usv ?a - area)
   (anon-box_at-uav11 ?b - box ?r - ugv)
   (anon-have_picture-uav11 ?d - disaster) (anon-store_of-uav11 ?s - store)
   (anon-at-uav11 ?a - area) (anon-have_water_sample-uav9 ?r - usv ?a - area)
   (anon-box_at-uav9 ?b - box ?r - ugv) (anon-have_picture-uav9 ?d - disaster)
   (anon-store_of-uav9 ?s - store) (anon-at-uav9 ?a - area)
   (anon-have_water_sample-uav3 ?r - usv ?a - area)
   (anon-box_at-uav3 ?b - box ?r - ugv) (anon-have_picture-uav3 ?d - disaster)
   (anon-store_of-uav3 ?s - store) (anon-at-uav3 ?a - area)
   (anon-have_water_sample-ugv11 ?r - usv ?a - area)
   (anon-box_at-ugv11 ?b - box) (anon-have_picture-ugv11 ?d - disaster)
   (anon-store_of-ugv11 ?s - store) (anon-at-ugv11 ?a - area)
   (anon-have_water_sample-ugv9 ?r - usv ?a - area) (anon-box_at-ugv9 ?b - box)
   (anon-have_picture-ugv9 ?d - disaster) (anon-store_of-ugv9 ?s - store)
   (anon-at-ugv9 ?a - area) (anon-have_water_sample-ugv7 ?r - usv ?a - area)
   (anon-box_at-ugv7 ?b - box) (anon-have_picture-ugv7 ?d - disaster)
   (anon-store_of-ugv7 ?s - store) (anon-at-ugv7 ?a - area)
   (anon-have_water_sample-ugv5 ?r - usv ?a - area) (anon-box_at-ugv5 ?b - box)
   (anon-have_picture-ugv5 ?d - disaster) (anon-store_of-ugv5 ?s - store)
   (anon-at-ugv5 ?a - area) (anon-have_water_sample-ugv3 ?r - usv ?a - area)
   (anon-box_at-ugv3 ?b - box) (anon-have_picture-ugv3 ?d - disaster)
   (anon-store_of-ugv3 ?s - store) (anon-at-ugv3 ?a - area)
   (anon-have_water_sample-usv11 ?a - area)
   (anon-box_at-usv11 ?b - box ?r - ugv)
   (anon-have_picture-usv11 ?d - disaster) (anon-store_of-usv11 ?s - store)
   (anon-at-usv11 ?a - area) (anon-have_water_sample-usv9 ?a - area)
   (anon-box_at-usv9 ?b - box ?r - ugv) (anon-have_picture-usv9 ?d - disaster)
   (anon-store_of-usv9 ?s - store) (anon-at-usv9 ?a - area)
   (anon-have_water_sample-usv5 ?a - area) (anon-box_at-usv5 ?b - box ?r - ugv)
   (anon-have_picture-usv5 ?d - disaster) (anon-store_of-usv5 ?s - store)
   (anon-at-usv5 ?a - area) (anon-at-usv3 ?a - area)
   (anon-store_of-usv3 ?s - store) (anon-have_picture-usv3 ?d - disaster)
   (anon-box_at-usv3 ?b - box ?r - ugv) (anon-have_water_sample-usv3 ?a - area))

  (:action anon-communicate_data-uav11 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-uav11 ?a1) (cdm_at ?c ?a2) (anon-have_picture-uav11 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-uav11 ?d)) (communicated_data ?d)))

  (:action anon-take_picture-uav11 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-uav11 ?a)) :effect
   (and (anon-have_picture-uav11 ?d)))

  (:action anon-navigate_uav-uav11 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-uav11 ?a1)) :effect
   (and (not (anon-at-uav11 ?a1)) (anon-at-uav11 ?a2)))

  (:action anon-communicate_data-uav3 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-uav3 ?a1) (cdm_at ?c ?a2) (anon-have_picture-uav3 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-uav3 ?d)) (communicated_data ?d)))

  (:action anon-take_picture-uav3 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-uav3 ?a)) :effect
   (and (anon-have_picture-uav3 ?d)))

  (:action anon-navigate_uav-uav3 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-uav3 ?a1)) :effect
   (and (not (anon-at-uav3 ?a1)) (anon-at-uav3 ?a2)))

  (:action anon-communicate_data-ugv9 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-ugv9 ?a1) (cdm_at ?c ?a2) (anon-have_picture-ugv9 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-ugv9 ?d)) (communicated_data ?d)))

  (:action anon-drop_box-ugv9 :parameters (?s - store ?a - area ?b - box)
   :precondition
   (and (anon-box_at-ugv9 ?b) (anon-store_of-ugv9 ?s) (anon-at-ugv9 ?a))
   :effect
   (and (not (full ?s)) (not (anon-box_at-ugv9 ?b)) (box_at_area ?b ?a)
        (empty ?s)))

  (:action anon-pickup_box-ugv9 :parameters
   (?s - store ?c - cdm ?a - area ?b - box) :precondition
   (and (box_at_cdm ?b ?c) (cdm_at ?c ?a) (anon-at-ugv9 ?a)
        (anon-store_of-ugv9 ?s) (empty ?s))
   :effect
   (and (not (empty ?s)) (not (box_at_cdm ?b ?c)) (anon-box_at-ugv9 ?b)
        (full ?s)))

  (:action anon-take_picture-ugv9 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-ugv9 ?a)) :effect
   (and (anon-have_picture-ugv9 ?d)))

  (:action anon-navigate_ugv-ugv9 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-ugv9 ?a1) (ground_path ?a1 ?a2)) :effect
   (and (not (anon-at-ugv9 ?a1)) (anon-at-ugv9 ?a2)))

  (:action anon-communicate_data-ugv5 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-ugv5 ?a1) (cdm_at ?c ?a2) (anon-have_picture-ugv5 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-ugv5 ?d)) (communicated_data ?d)))

  (:action anon-drop_box-ugv5 :parameters (?s - store ?a - area ?b - box)
   :precondition
   (and (anon-box_at-ugv5 ?b) (anon-store_of-ugv5 ?s) (anon-at-ugv5 ?a))
   :effect
   (and (not (full ?s)) (not (anon-box_at-ugv5 ?b)) (box_at_area ?b ?a)
        (empty ?s)))

  (:action anon-pickup_box-ugv5 :parameters
   (?s - store ?c - cdm ?a - area ?b - box) :precondition
   (and (box_at_cdm ?b ?c) (cdm_at ?c ?a) (anon-at-ugv5 ?a)
        (anon-store_of-ugv5 ?s) (empty ?s))
   :effect
   (and (not (empty ?s)) (not (box_at_cdm ?b ?c)) (anon-box_at-ugv5 ?b)
        (full ?s)))

  (:action anon-take_picture-ugv5 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-ugv5 ?a)) :effect
   (and (anon-have_picture-ugv5 ?d)))

  (:action anon-navigate_ugv-ugv5 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-ugv5 ?a1) (ground_path ?a1 ?a2)) :effect
   (and (not (anon-at-ugv5 ?a1)) (anon-at-ugv5 ?a2)))

  (:action anon-communicate_data-usv11 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-usv11 ?a1) (cdm_at ?c ?a2) (anon-have_picture-usv11 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-usv11 ?d)) (communicated_data ?d)))

  (:action anon-drop_sample-usv11 :parameters
   (?s - store ?a1 - area ?a2 - area ?c - cdm) :precondition
   (and (anon-store_of-usv11 ?s) (anon-have_water_sample-usv11 ?a2)
        (cdm_at ?c ?a1) (anon-at-usv11 ?a1))
   :effect
   (and (not (full ?s)) (not (anon-have_water_sample-usv11 ?a2))
        (have_water_sample_cdm ?c ?a2) (empty ?s)))

  (:action anon-sample_water-usv11 :parameters (?s - store ?a - area)
   :precondition (and (anon-at-usv11 ?a) (anon-store_of-usv11 ?s) (empty ?s))
   :effect (and (not (empty ?s)) (anon-have_water_sample-usv11 ?a) (full ?s)))

  (:action anon-take_picture-usv11 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-usv11 ?a)) :effect
   (and (anon-have_picture-usv11 ?d)))

  (:action anon-navigate_usv-usv11 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-usv11 ?a1) (water_path ?a1 ?a2)) :effect
   (and (not (anon-at-usv11 ?a1)) (anon-at-usv11 ?a2)))

  (:action anon-communicate_data-usv5 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-usv5 ?a1) (cdm_at ?c ?a2) (anon-have_picture-usv5 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-usv5 ?d)) (communicated_data ?d)))

  (:action anon-drop_sample-usv5 :parameters
   (?s - store ?a1 - area ?a2 - area ?c - cdm) :precondition
   (and (anon-store_of-usv5 ?s) (anon-have_water_sample-usv5 ?a2)
        (cdm_at ?c ?a1) (anon-at-usv5 ?a1))
   :effect
   (and (not (full ?s)) (not (anon-have_water_sample-usv5 ?a2))
        (have_water_sample_cdm ?c ?a2) (empty ?s)))

  (:action anon-sample_water-usv5 :parameters (?s - store ?a - area)
   :precondition (and (anon-at-usv5 ?a) (anon-store_of-usv5 ?s) (empty ?s))
   :effect (and (not (empty ?s)) (anon-have_water_sample-usv5 ?a) (full ?s)))

  (:action anon-take_picture-usv5 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-usv5 ?a)) :effect
   (and (anon-have_picture-usv5 ?d)))

  (:action anon-navigate_usv-usv5 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-usv5 ?a1) (water_path ?a1 ?a2)) :effect
   (and (not (anon-at-usv5 ?a1)) (anon-at-usv5 ?a2)))

  (:action anon-navigate_usv-usv3 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-usv3 ?a1) (water_path ?a1 ?a2)) :effect
   (and (not (anon-at-usv3 ?a1)) (anon-at-usv3 ?a2)))

  (:action anon-take_picture-usv3 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-usv3 ?a)) :effect
   (and (anon-have_picture-usv3 ?d)))

  (:action anon-sample_water-usv3 :parameters (?s - store ?a - area)
   :precondition (and (anon-at-usv3 ?a) (anon-store_of-usv3 ?s) (empty ?s))
   :effect (and (not (empty ?s)) (anon-have_water_sample-usv3 ?a) (full ?s)))

  (:action anon-drop_sample-usv3 :parameters
   (?s - store ?a1 - area ?a2 - area ?c - cdm) :precondition
   (and (anon-store_of-usv3 ?s) (anon-have_water_sample-usv3 ?a2)
        (cdm_at ?c ?a1) (anon-at-usv3 ?a1))
   :effect
   (and (not (full ?s)) (not (anon-have_water_sample-usv3 ?a2))
        (have_water_sample_cdm ?c ?a2) (empty ?s)))

  (:action anon-communicate_data-usv3 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-usv3 ?a1) (cdm_at ?c ?a2) (anon-have_picture-usv3 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-usv3 ?d)) (communicated_data ?d)))

  (:action anon-navigate_usv-usv9 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-usv9 ?a1) (water_path ?a1 ?a2)) :effect
   (and (not (anon-at-usv9 ?a1)) (anon-at-usv9 ?a2)))

  (:action anon-take_picture-usv9 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-usv9 ?a)) :effect
   (and (anon-have_picture-usv9 ?d)))

  (:action anon-sample_water-usv9 :parameters (?s - store ?a - area)
   :precondition (and (anon-at-usv9 ?a) (anon-store_of-usv9 ?s) (empty ?s))
   :effect (and (not (empty ?s)) (anon-have_water_sample-usv9 ?a) (full ?s)))

  (:action anon-drop_sample-usv9 :parameters
   (?s - store ?a1 - area ?a2 - area ?c - cdm) :precondition
   (and (anon-store_of-usv9 ?s) (anon-have_water_sample-usv9 ?a2)
        (cdm_at ?c ?a1) (anon-at-usv9 ?a1))
   :effect
   (and (not (full ?s)) (not (anon-have_water_sample-usv9 ?a2))
        (have_water_sample_cdm ?c ?a2) (empty ?s)))

  (:action anon-communicate_data-usv9 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-usv9 ?a1) (cdm_at ?c ?a2) (anon-have_picture-usv9 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-usv9 ?d)) (communicated_data ?d)))

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

  (:action anon-navigate_ugv-ugv7 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-ugv7 ?a1) (ground_path ?a1 ?a2)) :effect
   (and (not (anon-at-ugv7 ?a1)) (anon-at-ugv7 ?a2)))

  (:action anon-take_picture-ugv7 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-ugv7 ?a)) :effect
   (and (anon-have_picture-ugv7 ?d)))

  (:action anon-pickup_box-ugv7 :parameters
   (?s - store ?c - cdm ?a - area ?b - box) :precondition
   (and (box_at_cdm ?b ?c) (cdm_at ?c ?a) (anon-at-ugv7 ?a)
        (anon-store_of-ugv7 ?s) (empty ?s))
   :effect
   (and (not (empty ?s)) (not (box_at_cdm ?b ?c)) (anon-box_at-ugv7 ?b)
        (full ?s)))

  (:action anon-drop_box-ugv7 :parameters (?s - store ?a - area ?b - box)
   :precondition
   (and (anon-box_at-ugv7 ?b) (anon-store_of-ugv7 ?s) (anon-at-ugv7 ?a))
   :effect
   (and (not (full ?s)) (not (anon-box_at-ugv7 ?b)) (box_at_area ?b ?a)
        (empty ?s)))

  (:action anon-communicate_data-ugv7 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-ugv7 ?a1) (cdm_at ?c ?a2) (anon-have_picture-ugv7 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-ugv7 ?d)) (communicated_data ?d)))

  (:action anon-navigate_ugv-ugv11 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-ugv11 ?a1) (ground_path ?a1 ?a2)) :effect
   (and (not (anon-at-ugv11 ?a1)) (anon-at-ugv11 ?a2)))

  (:action anon-take_picture-ugv11 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-ugv11 ?a)) :effect
   (and (anon-have_picture-ugv11 ?d)))

  (:action anon-pickup_box-ugv11 :parameters
   (?s - store ?c - cdm ?a - area ?b - box) :precondition
   (and (box_at_cdm ?b ?c) (cdm_at ?c ?a) (anon-at-ugv11 ?a)
        (anon-store_of-ugv11 ?s) (empty ?s))
   :effect
   (and (not (empty ?s)) (not (box_at_cdm ?b ?c)) (anon-box_at-ugv11 ?b)
        (full ?s)))

  (:action anon-drop_box-ugv11 :parameters (?s - store ?a - area ?b - box)
   :precondition
   (and (anon-box_at-ugv11 ?b) (anon-store_of-ugv11 ?s) (anon-at-ugv11 ?a))
   :effect
   (and (not (full ?s)) (not (anon-box_at-ugv11 ?b)) (box_at_area ?b ?a)
        (empty ?s)))

  (:action anon-communicate_data-ugv11 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-ugv11 ?a1) (cdm_at ?c ?a2) (anon-have_picture-ugv11 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-ugv11 ?d)) (communicated_data ?d)))

  (:action anon-navigate_uav-uav9 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-uav9 ?a1)) :effect
   (and (not (anon-at-uav9 ?a1)) (anon-at-uav9 ?a2)))

  (:action anon-take_picture-uav9 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-uav9 ?a)) :effect
   (and (anon-have_picture-uav9 ?d)))

  (:action anon-communicate_data-uav9 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-uav9 ?a1) (cdm_at ?c ?a2) (anon-have_picture-uav9 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-uav9 ?d)) (communicated_data ?d)))

  (:action anon-navigate_usv-usv1 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-usv1 ?a1) (water_path ?a1 ?a2)) :effect
   (and (not (anon-at-usv1 ?a1)) (anon-at-usv1 ?a2)))

  (:action anon-take_picture-usv1 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-usv1 ?a)) :effect
   (and (anon-have_picture-usv1 ?d)))

  (:action anon-sample_water-usv1 :parameters (?s - store ?a - area)
   :precondition (and (anon-at-usv1 ?a) (anon-store_of-usv1 ?s) (empty ?s))
   :effect (and (not (empty ?s)) (anon-have_water_sample-usv1 ?a) (full ?s)))

  (:action anon-drop_sample-usv1 :parameters
   (?s - store ?a1 - area ?a2 - area ?c - cdm) :precondition
   (and (anon-store_of-usv1 ?s) (anon-have_water_sample-usv1 ?a2)
        (cdm_at ?c ?a1) (anon-at-usv1 ?a1))
   :effect
   (and (not (full ?s)) (not (anon-have_water_sample-usv1 ?a2))
        (have_water_sample_cdm ?c ?a2) (empty ?s)))

  (:action anon-communicate_data-usv1 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-usv1 ?a1) (cdm_at ?c ?a2) (anon-have_picture-usv1 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-usv1 ?d)) (communicated_data ?d)))
)