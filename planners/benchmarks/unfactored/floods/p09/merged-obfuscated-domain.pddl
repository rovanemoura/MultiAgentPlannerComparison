(define (domain floods)
  (:requirements :typing)
  (:types uav ugv usv - robot box cdm store disaster area robot - object)
  (:predicates (anon-have_water_sample-usv8 ?a - area)
   (anon-box_at-usv8 ?b - box ?r - ugv) (anon-have_picture-usv8 ?d - disaster)
   (anon-store_of-usv8 ?s - store) (anon-at-usv8 ?a - area)
   (in_range ?area - area ?area - area)
   (communicated_data ?disaster - disaster)
   (have_water_sample_cdm ?cdm - cdm ?area - area) (full ?store - store)
   (empty ?store - store) (visible_from ?disaster - disaster ?area - area)
   (ground_path ?area - area ?area - area)
   (water_path ?area - area ?area - area) (box_at_area ?box - box ?area - area)
   (box_at_cdm ?box - box ?cdm - cdm) (cdm_at ?cdm - cdm ?area - area)
   (anon-have_water_sample-uav10 ?r - usv ?a - area)
   (anon-box_at-uav10 ?b - box ?r - ugv)
   (anon-have_picture-uav10 ?d - disaster) (anon-store_of-uav10 ?s - store)
   (anon-at-uav10 ?a - area) (anon-have_water_sample-uav6 ?r - usv ?a - area)
   (anon-box_at-uav6 ?b - box ?r - ugv) (anon-have_picture-uav6 ?d - disaster)
   (anon-store_of-uav6 ?s - store) (anon-at-uav6 ?a - area)
   (anon-have_water_sample-ugv11 ?r - usv ?a - area)
   (anon-box_at-ugv11 ?b - box) (anon-have_picture-ugv11 ?d - disaster)
   (anon-store_of-ugv11 ?s - store) (anon-at-ugv11 ?a - area)
   (anon-have_water_sample-ugv9 ?r - usv ?a - area) (anon-box_at-ugv9 ?b - box)
   (anon-have_picture-ugv9 ?d - disaster) (anon-store_of-ugv9 ?s - store)
   (anon-at-ugv9 ?a - area) (anon-have_water_sample-ugv7 ?r - usv ?a - area)
   (anon-box_at-ugv7 ?b - box) (anon-have_picture-ugv7 ?d - disaster)
   (anon-store_of-ugv7 ?s - store) (anon-at-ugv7 ?a - area)
   (anon-have_water_sample-ugv3 ?r - usv ?a - area) (anon-box_at-ugv3 ?b - box)
   (anon-have_picture-ugv3 ?d - disaster) (anon-store_of-ugv3 ?s - store)
   (anon-at-ugv3 ?a - area) (anon-have_water_sample-usv11 ?a - area)
   (anon-box_at-usv11 ?b - box ?r - ugv)
   (anon-have_picture-usv11 ?d - disaster) (anon-store_of-usv11 ?s - store)
   (anon-at-usv11 ?a - area) (anon-have_water_sample-usv10 ?a - area)
   (anon-box_at-usv10 ?b - box ?r - ugv)
   (anon-have_picture-usv10 ?d - disaster) (anon-store_of-usv10 ?s - store)
   (anon-at-usv10 ?a - area) (anon-have_water_sample-usv6 ?a - area)
   (anon-box_at-usv6 ?b - box ?r - ugv) (anon-have_picture-usv6 ?d - disaster)
   (anon-store_of-usv6 ?s - store) (anon-at-usv6 ?a - area)
   (anon-at-usv4 ?a - area) (anon-store_of-usv4 ?s - store)
   (anon-have_picture-usv4 ?d - disaster) (anon-box_at-usv4 ?b - box ?r - ugv)
   (anon-have_water_sample-usv4 ?a - area))

  (:action anon-communicate_data-uav10 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-uav10 ?a1) (cdm_at ?c ?a2) (anon-have_picture-uav10 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-uav10 ?d)) (communicated_data ?d)))

  (:action anon-take_picture-uav10 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-uav10 ?a)) :effect
   (and (anon-have_picture-uav10 ?d)))

  (:action anon-navigate_uav-uav10 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-uav10 ?a1)) :effect
   (and (not (anon-at-uav10 ?a1)) (anon-at-uav10 ?a2)))

  (:action anon-communicate_data-ugv11 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-ugv11 ?a1) (cdm_at ?c ?a2) (anon-have_picture-ugv11 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-ugv11 ?d)) (communicated_data ?d)))

  (:action anon-drop_box-ugv11 :parameters (?s - store ?a - area ?b - box)
   :precondition
   (and (anon-box_at-ugv11 ?b) (anon-store_of-ugv11 ?s) (anon-at-ugv11 ?a))
   :effect
   (and (not (full ?s)) (not (anon-box_at-ugv11 ?b)) (box_at_area ?b ?a)
        (empty ?s)))

  (:action anon-pickup_box-ugv11 :parameters
   (?s - store ?c - cdm ?a - area ?b - box) :precondition
   (and (box_at_cdm ?b ?c) (cdm_at ?c ?a) (anon-at-ugv11 ?a)
        (anon-store_of-ugv11 ?s) (empty ?s))
   :effect
   (and (not (empty ?s)) (not (box_at_cdm ?b ?c)) (anon-box_at-ugv11 ?b)
        (full ?s)))

  (:action anon-take_picture-ugv11 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-ugv11 ?a)) :effect
   (and (anon-have_picture-ugv11 ?d)))

  (:action anon-navigate_ugv-ugv11 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-ugv11 ?a1) (ground_path ?a1 ?a2)) :effect
   (and (not (anon-at-ugv11 ?a1)) (anon-at-ugv11 ?a2)))

  (:action anon-communicate_data-ugv7 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-ugv7 ?a1) (cdm_at ?c ?a2) (anon-have_picture-ugv7 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-ugv7 ?d)) (communicated_data ?d)))

  (:action anon-drop_box-ugv7 :parameters (?s - store ?a - area ?b - box)
   :precondition
   (and (anon-box_at-ugv7 ?b) (anon-store_of-ugv7 ?s) (anon-at-ugv7 ?a))
   :effect
   (and (not (full ?s)) (not (anon-box_at-ugv7 ?b)) (box_at_area ?b ?a)
        (empty ?s)))

  (:action anon-pickup_box-ugv7 :parameters
   (?s - store ?c - cdm ?a - area ?b - box) :precondition
   (and (box_at_cdm ?b ?c) (cdm_at ?c ?a) (anon-at-ugv7 ?a)
        (anon-store_of-ugv7 ?s) (empty ?s))
   :effect
   (and (not (empty ?s)) (not (box_at_cdm ?b ?c)) (anon-box_at-ugv7 ?b)
        (full ?s)))

  (:action anon-take_picture-ugv7 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-ugv7 ?a)) :effect
   (and (anon-have_picture-ugv7 ?d)))

  (:action anon-navigate_ugv-ugv7 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-ugv7 ?a1) (ground_path ?a1 ?a2)) :effect
   (and (not (anon-at-ugv7 ?a1)) (anon-at-ugv7 ?a2)))

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

  (:action anon-navigate_usv-usv10 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-usv10 ?a1) (water_path ?a1 ?a2)) :effect
   (and (not (anon-at-usv10 ?a1)) (anon-at-usv10 ?a2)))

  (:action anon-take_picture-usv10 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-usv10 ?a)) :effect
   (and (anon-have_picture-usv10 ?d)))

  (:action anon-sample_water-usv10 :parameters (?s - store ?a - area)
   :precondition (and (anon-at-usv10 ?a) (anon-store_of-usv10 ?s) (empty ?s))
   :effect (and (not (empty ?s)) (full ?s) (anon-have_water_sample-usv10 ?a)))

  (:action anon-drop_sample-usv10 :parameters
   (?s - store ?a1 - area ?a2 - area ?c - cdm) :precondition
   (and (anon-store_of-usv10 ?s) (anon-have_water_sample-usv10 ?a2)
        (cdm_at ?c ?a1) (anon-at-usv10 ?a1))
   :effect
   (and (not (full ?s)) (not (anon-have_water_sample-usv10 ?a2)) (empty ?s)
        (have_water_sample_cdm ?c ?a2)))

  (:action anon-communicate_data-usv10 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-usv10 ?a1) (cdm_at ?c ?a2) (anon-have_picture-usv10 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-usv10 ?d)) (communicated_data ?d)))

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

  (:action anon-navigate_uav-uav6 :parameters (?a1 - area ?a2 - area)
   :precondition (and (anon-at-uav6 ?a1)) :effect
   (and (not (anon-at-uav6 ?a1)) (anon-at-uav6 ?a2)))

  (:action anon-take_picture-uav6 :parameters (?a - area ?d - disaster)
   :precondition (and (visible_from ?d ?a) (anon-at-uav6 ?a)) :effect
   (and (anon-have_picture-uav6 ?d)))

  (:action anon-communicate_data-uav6 :parameters
   (?c - cdm ?d - disaster ?a1 - area ?a2 - area) :precondition
   (and (anon-at-uav6 ?a1) (cdm_at ?c ?a2) (anon-have_picture-uav6 ?d)
        (in_range ?a1 ?a2))
   :effect (and (not (anon-have_picture-uav6 ?d)) (communicated_data ?d)))

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
)