(define (domain floods)
  (:requirements :typing)
  (:types usv ugv uav - robot robot area disaster store cdm box - object)
  (:predicates (anon-have_water_sample-ugv7 ?r - usv ?a - area)
   (anon-box_at-ugv7 ?b - box) (anon-have_picture-ugv7 ?d - disaster)
   (anon-store_of-ugv7 ?s - store) (anon-at-ugv7 ?a - area)
   (in_range ?area - area ?area - area)
   (communicated_data ?disaster - disaster)
   (have_water_sample_cdm ?cdm - cdm ?area - area) (full ?store - store)
   (empty ?store - store) (visible_from ?disaster - disaster ?area - area)
   (ground_path ?area - area ?area - area)
   (water_path ?area - area ?area - area) (box_at_area ?box - box ?area - area)
   (box_at_cdm ?box - box ?cdm - cdm) (cdm_at ?cdm - cdm ?area - area)
   (anon-have_water_sample-uav8 ?r - usv ?a - area)
   (anon-box_at-uav8 ?b - box ?r - ugv) (anon-have_picture-uav8 ?d - disaster)
   (anon-store_of-uav8 ?s - store) (anon-at-uav8 ?a - area)
   (anon-have_water_sample-ugv9 ?r - usv ?a - area) (anon-box_at-ugv9 ?b - box)
   (anon-have_picture-ugv9 ?d - disaster) (anon-store_of-ugv9 ?s - store)
   (anon-at-ugv9 ?a - area) (anon-have_water_sample-ugv5 ?r - usv ?a - area)
   (anon-box_at-ugv5 ?b - box) (anon-have_picture-ugv5 ?d - disaster)
   (anon-store_of-ugv5 ?s - store) (anon-at-ugv5 ?a - area)
   (anon-have_water_sample-ugv3 ?r - usv ?a - area) (anon-box_at-ugv3 ?b - box)
   (anon-have_picture-ugv3 ?d - disaster) (anon-store_of-ugv3 ?s - store)
   (anon-at-ugv3 ?a - area) (anon-have_water_sample-ugv1 ?r - usv ?a - area)
   (anon-box_at-ugv1 ?b - box) (anon-have_picture-ugv1 ?d - disaster)
   (anon-store_of-ugv1 ?s - store) (anon-at-ugv1 ?a - area)
   (anon-have_water_sample-usv8 ?a - area) (anon-box_at-usv8 ?b - box ?r - ugv)
   (anon-have_picture-usv8 ?d - disaster) (anon-store_of-usv8 ?s - store)
   (anon-at-usv8 ?a - area) (anon-have_water_sample-usv6 ?a - area)
   (anon-box_at-usv6 ?b - box ?r - ugv) (anon-have_picture-usv6 ?d - disaster)
   (anon-store_of-usv6 ?s - store) (anon-at-usv6 ?a - area)
   (anon-have_water_sample-usv4 ?a - area) (anon-box_at-usv4 ?b - box ?r - ugv)
   (anon-have_picture-usv4 ?d - disaster) (anon-store_of-usv4 ?s - store)
   (anon-at-usv4 ?a - area) (anon-at-usv2 ?a - area)
   (anon-store_of-usv2 ?s - store) (anon-have_picture-usv2 ?d - disaster)
   (anon-box_at-usv2 ?b - box ?r - ugv) (anon-have_water_sample-usv2 ?a - area))

  (:action anon-communicate_data-uav8 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-uav8 ?a1) (cdm_at ?c ?a2) (anon-have_picture-uav8 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-uav8 ?d)) (communicated_data ?d)))

  (:action anon-take_picture-uav8 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-uav8 ?a)) :effect
   (and (anon-have_picture-uav8 ?d)))

  (:action anon-navigate_uav-uav8 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-uav8 ?a1)) :effect
   (and (not (anon-at-uav8 ?a1)) (anon-at-uav8 ?a2)))

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

  (:action anon-communicate_data-ugv1 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-ugv1 ?a1) (cdm_at ?c ?a2) (anon-have_picture-ugv1 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-ugv1 ?d)) (communicated_data ?d)))

  (:action anon-drop_box-ugv1 :parameters (?s - store ?a - area ?b - box)
   :precondition
   (and (anon-box_at-ugv1 ?b) (anon-store_of-ugv1 ?s) (anon-at-ugv1 ?a))
   :effect
   (and (not (full ?s)) (not (anon-box_at-ugv1 ?b)) (box_at_area ?b ?a)
        (empty ?s)))

  (:action anon-pickup_box-ugv1 :parameters
   (?s - store ?c - cdm ?a - area ?b - box) :precondition
   (and (box_at_cdm ?b ?c) (cdm_at ?c ?a) (anon-at-ugv1 ?a)
        (anon-store_of-ugv1 ?s) (empty ?s))
   :effect
   (and (not (empty ?s)) (not (box_at_cdm ?b ?c)) (anon-box_at-ugv1 ?b)
        (full ?s)))

  (:action anon-take_picture-ugv1 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-ugv1 ?a)) :effect
   (and (anon-have_picture-ugv1 ?d)))

  (:action anon-navigate_ugv-ugv1 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-ugv1 ?a1) (ground_path ?a1 ?a2)) :effect
   (and (not (anon-at-ugv1 ?a1)) (anon-at-ugv1 ?a2)))

  (:action anon-communicate_data-usv6 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-usv6 ?a1) (cdm_at ?c ?a2) (anon-have_picture-usv6 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-usv6 ?d)) (communicated_data ?d)))

  (:action anon-drop_sample-usv6 :parameters
   (?s - store ?a1 - area ?a2 - area ?c - cdm) :precondition
   (and (anon-store_of-usv6 ?s) (anon-have_water_sample-usv6 ?a2)
        (cdm_at ?c ?a1) (anon-at-usv6 ?a1))
   :effect
   (and (not (full ?s)) (not (anon-have_water_sample-usv6 ?a2)) (empty ?s)
        (have_water_sample_cdm ?c ?a2)))

  (:action anon-sample_water-usv6 :parameters (?s - store ?a - area)
   :precondition (and (anon-at-usv6 ?a) (anon-store_of-usv6 ?s) (empty ?s))
   :effect (and (not (empty ?s)) (full ?s) (anon-have_water_sample-usv6 ?a)))

  (:action anon-take_picture-usv6 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-usv6 ?a)) :effect
   (and (anon-have_picture-usv6 ?d)))

  (:action anon-navigate_usv-usv6 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-usv6 ?a1) (water_path ?a1 ?a2)) :effect
   (and (not (anon-at-usv6 ?a1)) (anon-at-usv6 ?a2)))

  (:action anon-communicate_data-usv2 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-usv2 ?a1) (cdm_at ?c ?a2) (anon-have_picture-usv2 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-usv2 ?d)) (communicated_data ?d)))

  (:action anon-drop_sample-usv2 :parameters
   (?s - store ?a1 - area ?a2 - area ?c - cdm) :precondition
   (and (anon-store_of-usv2 ?s) (anon-have_water_sample-usv2 ?a2)
        (cdm_at ?c ?a1) (anon-at-usv2 ?a1))
   :effect
   (and (not (full ?s)) (not (anon-have_water_sample-usv2 ?a2)) (empty ?s)
        (have_water_sample_cdm ?c ?a2)))

  (:action anon-sample_water-usv2 :parameters (?s - store ?a - area)
   :precondition (and (anon-at-usv2 ?a) (anon-store_of-usv2 ?s) (empty ?s))
   :effect (and (not (empty ?s)) (full ?s) (anon-have_water_sample-usv2 ?a)))

  (:action anon-take_picture-usv2 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-usv2 ?a)) :effect
   (and (anon-have_picture-usv2 ?d)))

  (:action anon-navigate_usv-usv2 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-usv2 ?a1) (water_path ?a1 ?a2)) :effect
   (and (not (anon-at-usv2 ?a1)) (anon-at-usv2 ?a2)))

  (:action anon-navigate_usv-usv4 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-usv4 ?a1) (water_path ?a1 ?a2)) :effect
   (and (not (anon-at-usv4 ?a1)) (anon-at-usv4 ?a2)))

  (:action anon-take_picture-usv4 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-usv4 ?a)) :effect
   (and (anon-have_picture-usv4 ?d)))

  (:action anon-sample_water-usv4 :parameters (?s - store ?a - area)
   :precondition (and (anon-at-usv4 ?a) (anon-store_of-usv4 ?s) (empty ?s))
   :effect (and (not (empty ?s)) (full ?s) (anon-have_water_sample-usv4 ?a)))

  (:action anon-drop_sample-usv4 :parameters
   (?s - store ?a1 - area ?a2 - area ?c - cdm) :precondition
   (and (anon-store_of-usv4 ?s) (anon-have_water_sample-usv4 ?a2)
        (cdm_at ?c ?a1) (anon-at-usv4 ?a1))
   :effect
   (and (not (full ?s)) (not (anon-have_water_sample-usv4 ?a2)) (empty ?s)
        (have_water_sample_cdm ?c ?a2)))

  (:action anon-communicate_data-usv4 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-usv4 ?a1) (cdm_at ?c ?a2) (anon-have_picture-usv4 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-usv4 ?d)) (communicated_data ?d)))

  (:action anon-navigate_usv-usv8 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-usv8 ?a1) (water_path ?a1 ?a2)) :effect
   (and (not (anon-at-usv8 ?a1)) (anon-at-usv8 ?a2)))

  (:action anon-take_picture-usv8 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-usv8 ?a)) :effect
   (and (anon-have_picture-usv8 ?d)))

  (:action anon-sample_water-usv8 :parameters (?s - store ?a - area)
   :precondition (and (anon-at-usv8 ?a) (anon-store_of-usv8 ?s) (empty ?s))
   :effect (and (not (empty ?s)) (full ?s) (anon-have_water_sample-usv8 ?a)))

  (:action anon-drop_sample-usv8 :parameters
   (?s - store ?a1 - area ?a2 - area ?c - cdm) :precondition
   (and (anon-store_of-usv8 ?s) (anon-have_water_sample-usv8 ?a2)
        (cdm_at ?c ?a1) (anon-at-usv8 ?a1))
   :effect
   (and (not (full ?s)) (not (anon-have_water_sample-usv8 ?a2)) (empty ?s)
        (have_water_sample_cdm ?c ?a2)))

  (:action anon-communicate_data-usv8 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-usv8 ?a1) (cdm_at ?c ?a2) (anon-have_picture-usv8 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-usv8 ?d)) (communicated_data ?d)))

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

  (:action anon-navigate_ugv-ugv9 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-ugv9 ?a1) (ground_path ?a1 ?a2)) :effect
   (and (not (anon-at-ugv9 ?a1)) (anon-at-ugv9 ?a2)))

  (:action anon-take_picture-ugv9 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-ugv9 ?a)) :effect
   (and (anon-have_picture-ugv9 ?d)))

  (:action anon-pickup_box-ugv9 :parameters
   (?s - store ?c - cdm ?a - area ?b - box) :precondition
   (and (box_at_cdm ?b ?c) (cdm_at ?c ?a) (anon-at-ugv9 ?a)
        (anon-store_of-ugv9 ?s) (empty ?s))
   :effect
   (and (not (empty ?s)) (not (box_at_cdm ?b ?c)) (anon-box_at-ugv9 ?b)
        (full ?s)))

  (:action anon-drop_box-ugv9 :parameters (?s - store ?a - area ?b - box)
   :precondition
   (and (anon-box_at-ugv9 ?b) (anon-store_of-ugv9 ?s) (anon-at-ugv9 ?a))
   :effect
   (and (not (full ?s)) (not (anon-box_at-ugv9 ?b)) (box_at_area ?b ?a)
        (empty ?s)))

  (:action anon-communicate_data-ugv9 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-ugv9 ?a1) (cdm_at ?c ?a2) (anon-have_picture-ugv9 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-ugv9 ?d)) (communicated_data ?d)))

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
)