(define (domain rover)
  (:requirements :typing)
  (:types rover1-store rover0-store - store rover1-camera rover0-camera -
   camera rover waypoint store camera mode lander objective)
  (:predicates (at ?x - rover ?y - waypoint) (rover0-at ?y - waypoint)
   (rover1-at ?y - waypoint) (at_lander ?x - lander ?y - waypoint)
   (can_traverse ?r - rover ?x - waypoint ?y - waypoint)
   (rover0-can_traverse ?x - waypoint ?y - waypoint)
   (rover1-can_traverse ?x - waypoint ?y - waypoint)
   (equipped_for_soil_analysis ?r - rover) (rover0-equipped_for_soil_analysis)
   (rover1-equipped_for_soil_analysis) (equipped_for_rock_analysis ?r - rover)
   (rover0-equipped_for_rock_analysis) (rover1-equipped_for_rock_analysis)
   (equipped_for_imaging ?r - rover) (rover0-equipped_for_imaging)
   (rover1-equipped_for_imaging) (empty ?s - store)
   (rover0-empty ?s - rover0-store) (rover1-empty ?s - rover1-store)
   (have_rock_analysis ?r - rover ?w - waypoint)
   (rover0-have_rock_analysis ?w - waypoint)
   (rover1-have_rock_analysis ?w - waypoint)
   (have_soil_analysis ?r - rover ?w - waypoint)
   (rover0-have_soil_analysis ?w - waypoint)
   (rover1-have_soil_analysis ?w - waypoint) (full ?s - store)
   (rover0-full ?s - rover0-store) (rover1-full ?s - rover1-store)
   (calibrated ?c - camera ?r - rover) (rover0-calibrated ?c - rover0-camera)
   (rover1-calibrated ?c - rover1-camera) (supports ?c - camera ?m - mode)
   (rover0-supports ?c - rover0-camera ?m - mode)
   (rover1-supports ?c - rover1-camera ?m - mode) (available ?r - rover)
   (rover0-available) (rover1-available) (visible ?w - waypoint ?p - waypoint)
   (have_image ?r - rover ?o - objective ?m - mode)
   (rover0-have_image ?o - objective ?m - mode)
   (rover1-have_image ?o - objective ?m - mode)
   (communicated_soil_data ?w - waypoint)
   (communicated_rock_data ?w - waypoint)
   (communicated_image_data ?o - objective ?m - mode)
   (at_soil_sample ?w - waypoint) (at_rock_sample ?w - waypoint)
   (visible_from ?o - objective ?w - waypoint)
   (rover0-visible_from ?o - objective ?w - waypoint)
   (rover1-visible_from ?o - objective ?w - waypoint)
   (store_of ?s - store ?r - rover) (rover0-store_of ?s - rover0-store)
   (rover1-store_of ?s - rover1-store)
   (calibration_target ?i - camera ?o - objective)
   (rover0-calibration_target ?i - rover0-camera ?o - objective)
   (rover1-calibration_target ?i - rover1-camera ?o - objective)
   (on_board ?i - camera ?r - rover) (rover0-on_board ?i - rover0-camera)
   (rover1-on_board ?i - rover1-camera) (channel_free ?l - lander))

  (:action navigate :parameters (?x - rover ?y - waypoint ?z - waypoint)
   :precondition
   (and (can_traverse ?x ?y ?z) (available ?x) (at ?x ?y) (visible ?y ?z))
   :effect (and (not (at ?x ?y)) (at ?x ?z)))

  (:action sample_soil :parameters (?x - rover ?s - store ?p - waypoint)
   :precondition
   (and (at ?x ?p) (at_soil_sample ?p) (equipped_for_soil_analysis ?x)
        (store_of ?s ?x) (empty ?s))
   :effect
   (and (not (empty ?s)) (full ?s) (have_soil_analysis ?x ?p)
        (not (at_soil_sample ?p))))

  (:action sample_rock :parameters (?x - rover ?s - store ?p - waypoint)
   :precondition
   (and (at ?x ?p) (at_rock_sample ?p) (equipped_for_rock_analysis ?x)
        (store_of ?s ?x) (empty ?s))
   :effect
   (and (not (empty ?s)) (full ?s) (have_rock_analysis ?x ?p)
        (not (at_rock_sample ?p))))

  (:action drop :parameters (?x - rover ?y - store) :precondition
   (and (store_of ?y ?x) (full ?y)) :effect (and (not (full ?y)) (empty ?y)))

  (:action calibrate :parameters
   (?r - rover ?i - camera ?t - objective ?w - waypoint) :precondition
   (and (equipped_for_imaging ?r) (calibration_target ?i ?t) (at ?r ?w)
        (visible_from ?t ?w) (on_board ?i ?r))
   :effect (calibrated ?i ?r))

  (:action take_image :parameters
   (?r - rover ?p - waypoint ?o - objective ?i - camera ?m - mode)
   :precondition
   (and (calibrated ?i ?r) (on_board ?i ?r) (equipped_for_imaging ?r)
        (supports ?i ?m) (visible_from ?o ?p) (at ?r ?p))
   :effect (and (have_image ?r ?o ?m) (not (calibrated ?i ?r))))

  (:action communicate_soil_data :parameters
   (?r - rover ?l - lander ?p - waypoint ?x - waypoint ?y - waypoint)
   :precondition
   (and (at ?r ?x) (at_lander ?l ?y) (have_soil_analysis ?r ?p) (visible ?x ?y)
        (available ?r) (channel_free ?l))
   :effect (and (communicated_soil_data ?p)))

  (:action communicate_rock_data :parameters
   (?r - rover ?l - lander ?p - waypoint ?x - waypoint ?y - waypoint)
   :precondition
   (and (at ?r ?x) (at_lander ?l ?y) (have_rock_analysis ?r ?p) (visible ?x ?y)
        (available ?r) (channel_free ?l))
   :effect (and (communicated_rock_data ?p)))

  (:action c_i_d :parameters
   (?r - rover ?l - lander ?o - objective ?m - mode ?x - waypoint ?y -
    waypoint)
   :precondition
   (and (at ?r ?x) (at_lander ?l ?y) (have_image ?r ?o ?m) (visible ?x ?y)
        (available ?r) (channel_free ?l))
   :effect (and (communicated_image_data ?o ?m)))
)