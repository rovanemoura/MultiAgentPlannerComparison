(define (domain rover)
  (:requirements :typing)
  (:types rover waypoint store camera mode lander objective)
  (:constants rover0 - rover anon-rover0-18 - store general - lander waypoint4
   waypoint1 waypoint3 waypoint6 waypoint0 waypoint5 - waypoint)
  (:predicates (channel_free ?l - lander) (on_board ?i - camera ?r - rover)
   (calibration_target ?i - camera ?o - objective)
   (store_of ?s - store ?r - rover) (visible_from ?o - objective ?w - waypoint)
   (at_rock_sample ?w - waypoint) (at_soil_sample ?w - waypoint)
   (communicated_image_data ?o - objective ?m - mode)
   (communicated_rock_data ?w - waypoint)
   (communicated_soil_data ?w - waypoint)
   (have_image ?r - rover ?o - objective ?m - mode)
   (visible ?w - waypoint ?p - waypoint) (available ?r - rover)
   (supports ?c - camera ?m - mode) (calibrated ?c - camera ?r - rover)
   (full ?s - store) (have_soil_analysis ?r - rover ?w - waypoint)
   (have_rock_analysis ?r - rover ?w - waypoint) (empty ?s - store)
   (equipped_for_imaging ?r - rover) (equipped_for_rock_analysis ?r - rover)
   (equipped_for_soil_analysis ?r - rover)
   (can_traverse ?r - rover ?x - waypoint ?y - waypoint)
   (at_lander ?x - lander ?y - waypoint) (at ?x - rover ?y - waypoint)
   (anon-rover0-14 ?s - store ?r - rover) (anon-rover0-12 ?r - rover)
   (anon-rover0-9 ?s - store) (anon-rover0-8 ?r - rover ?w - waypoint)
   (anon-rover0-7 ?r - rover ?w - waypoint) (anon-rover0-6 ?s - store)
   (anon-rover0-4 ?r - rover) (anon-rover0-3 ?r - rover)
   (anon-rover0-2 ?r - rover ?x - waypoint ?y - waypoint)
   (anon-rover0-1 ?x - rover ?y - waypoint))

  (:action communicate_rock_data---rover0---rover0---general---waypoint0---waypoint5---waypoint1
   :parameters ()
   :precondition (and (channel_free general) (anon-rover0-12 rover0)
                      (visible waypoint5 waypoint1)
                      (anon-rover0-7 rover0 waypoint0)
                      (at_lander general waypoint1)
                      (anon-rover0-1 rover0 waypoint5))
    :effect (and (communicated_rock_data waypoint0)))

  (:action communicate_rock_data---rover0---rover0---general---waypoint1---waypoint5---waypoint1
   :parameters ()
   :precondition (and (channel_free general) (anon-rover0-12 rover0)
                      (visible waypoint5 waypoint1)
                      (anon-rover0-7 rover0 waypoint1)
                      (at_lander general waypoint1)
                      (anon-rover0-1 rover0 waypoint5))
    :effect (and (communicated_rock_data waypoint1)))

  (:action navigate---rover0---rover0---waypoint1---waypoint5
   :parameters ()
   :precondition (and (visible waypoint1 waypoint5)
                      (anon-rover0-1 rover0 waypoint1) (anon-rover0-12 rover0)
                      (anon-rover0-2 rover0 waypoint1 waypoint5))
    :effect (and (not (anon-rover0-1 rover0 waypoint1))
                 (anon-rover0-1 rover0 waypoint5)))

  (:action sample_rock---rover0---rover0---anon-rover0-18---waypoint1
   :parameters ()
   :precondition (and (anon-rover0-6 anon-rover0-18)
                      (anon-rover0-14 anon-rover0-18 rover0)
                      (anon-rover0-4 rover0) (at_rock_sample waypoint1)
                      (anon-rover0-1 rover0 waypoint1))
    :effect (and (not (anon-rover0-6 anon-rover0-18))
                 (not (at_rock_sample waypoint1))
                 (anon-rover0-7 rover0 waypoint1)
                 (anon-rover0-9 anon-rover0-18)))

  (:action navigate---rover0---rover0---waypoint4---waypoint1
   :parameters ()
   :precondition (and (visible waypoint4 waypoint1)
                      (anon-rover0-1 rover0 waypoint4) (anon-rover0-12 rover0)
                      (anon-rover0-2 rover0 waypoint4 waypoint1))
    :effect (and (not (anon-rover0-1 rover0 waypoint4))
                 (anon-rover0-1 rover0 waypoint1)))

  (:action navigate---rover0---rover0---waypoint0---waypoint4
   :parameters ()
   :precondition (and (visible waypoint0 waypoint4)
                      (anon-rover0-1 rover0 waypoint0) (anon-rover0-12 rover0)
                      (anon-rover0-2 rover0 waypoint0 waypoint4))
    :effect (and (not (anon-rover0-1 rover0 waypoint0))
                 (anon-rover0-1 rover0 waypoint4)))

  (:action sample_rock---rover0---rover0---anon-rover0-18---waypoint0
   :parameters ()
   :precondition (and (anon-rover0-6 anon-rover0-18)
                      (anon-rover0-14 anon-rover0-18 rover0)
                      (anon-rover0-4 rover0) (at_rock_sample waypoint0)
                      (anon-rover0-1 rover0 waypoint0))
    :effect (and (not (anon-rover0-6 anon-rover0-18))
                 (not (at_rock_sample waypoint0))
                 (anon-rover0-7 rover0 waypoint0)
                 (anon-rover0-9 anon-rover0-18)))

  (:action navigate---rover0---rover0---waypoint4---waypoint0
   :parameters ()
   :precondition (and (visible waypoint4 waypoint0)
                      (anon-rover0-1 rover0 waypoint4) (anon-rover0-12 rover0)
                      (anon-rover0-2 rover0 waypoint4 waypoint0))
    :effect (and (not (anon-rover0-1 rover0 waypoint4))
                 (anon-rover0-1 rover0 waypoint0)))

  (:action communicate_soil_data---rover0---rover0---general---waypoint6---waypoint4---waypoint1
   :parameters ()
   :precondition (and (channel_free general) (anon-rover0-12 rover0)
                      (visible waypoint4 waypoint1)
                      (anon-rover0-8 rover0 waypoint6)
                      (at_lander general waypoint1)
                      (anon-rover0-1 rover0 waypoint4))
    :effect (and (communicated_soil_data waypoint6)))

  (:action navigate---rover0---rover0---waypoint6---waypoint4
   :parameters ()
   :precondition (and (visible waypoint6 waypoint4)
                      (anon-rover0-1 rover0 waypoint6) (anon-rover0-12 rover0)
                      (anon-rover0-2 rover0 waypoint6 waypoint4))
    :effect (and (not (anon-rover0-1 rover0 waypoint6))
                 (anon-rover0-1 rover0 waypoint4)))

  (:action sample_soil---rover0---rover0---anon-rover0-18---waypoint6
   :parameters ()
   :precondition (and (anon-rover0-6 anon-rover0-18)
                      (anon-rover0-14 anon-rover0-18 rover0)
                      (anon-rover0-3 rover0) (at_soil_sample waypoint6)
                      (anon-rover0-1 rover0 waypoint6))
    :effect (and (not (anon-rover0-6 anon-rover0-18))
                 (not (at_soil_sample waypoint6))
                 (anon-rover0-8 rover0 waypoint6)
                 (anon-rover0-9 anon-rover0-18)))

  (:action navigate---rover0---rover0---waypoint4---waypoint6
   :parameters ()
   :precondition (and (visible waypoint4 waypoint6)
                      (anon-rover0-1 rover0 waypoint4) (anon-rover0-12 rover0)
                      (anon-rover0-2 rover0 waypoint4 waypoint6))
    :effect (and (not (anon-rover0-1 rover0 waypoint4))
                 (anon-rover0-1 rover0 waypoint6)))

  (:action communicate_soil_data---rover0---rover0---general---waypoint3---waypoint4---waypoint1
   :parameters ()
   :precondition (and (channel_free general) (anon-rover0-12 rover0)
                      (visible waypoint4 waypoint1)
                      (anon-rover0-8 rover0 waypoint3)
                      (at_lander general waypoint1)
                      (anon-rover0-1 rover0 waypoint4))
    :effect (and (communicated_soil_data waypoint3)))

  (:action sample_soil---rover0---rover0---anon-rover0-18---waypoint3
   :parameters ()
   :precondition (and (anon-rover0-6 anon-rover0-18)
                      (anon-rover0-14 anon-rover0-18 rover0)
                      (anon-rover0-3 rover0) (at_soil_sample waypoint3)
                      (anon-rover0-1 rover0 waypoint3))
    :effect (and (not (anon-rover0-6 anon-rover0-18))
                 (not (at_soil_sample waypoint3))
                 (anon-rover0-8 rover0 waypoint3)
                 (anon-rover0-9 anon-rover0-18)))

  (:action communicate_rock_data---rover0---rover0---general---waypoint3---waypoint4---waypoint1
   :parameters ()
   :precondition (and (channel_free general) (anon-rover0-12 rover0)
                      (visible waypoint4 waypoint1)
                      (anon-rover0-7 rover0 waypoint3)
                      (at_lander general waypoint1)
                      (anon-rover0-1 rover0 waypoint4))
    :effect (and (communicated_rock_data waypoint3)))

  (:action navigate---rover0---rover0---waypoint3---waypoint4
   :parameters ()
   :precondition (and (visible waypoint3 waypoint4)
                      (anon-rover0-1 rover0 waypoint3) (anon-rover0-12 rover0)
                      (anon-rover0-2 rover0 waypoint3 waypoint4))
    :effect (and (not (anon-rover0-1 rover0 waypoint3))
                 (anon-rover0-1 rover0 waypoint4)))

  (:action sample_rock---rover0---rover0---anon-rover0-18---waypoint3
   :parameters ()
   :precondition (and (anon-rover0-6 anon-rover0-18)
                      (anon-rover0-14 anon-rover0-18 rover0)
                      (anon-rover0-4 rover0) (at_rock_sample waypoint3)
                      (anon-rover0-1 rover0 waypoint3))
    :effect (and (not (anon-rover0-6 anon-rover0-18))
                 (not (at_rock_sample waypoint3))
                 (anon-rover0-7 rover0 waypoint3)
                 (anon-rover0-9 anon-rover0-18)))

  (:action navigate---rover0---rover0---waypoint4---waypoint3
   :parameters ()
   :precondition (and (visible waypoint4 waypoint3)
                      (anon-rover0-1 rover0 waypoint4) (anon-rover0-12 rover0)
                      (anon-rover0-2 rover0 waypoint4 waypoint3))
    :effect (and (not (anon-rover0-1 rover0 waypoint4))
                 (anon-rover0-1 rover0 waypoint3)))

  (:action communicate_soil_data---rover0---rover0---general---waypoint4---waypoint4---waypoint1
   :parameters ()
   :precondition (and (channel_free general) (anon-rover0-12 rover0)
                      (visible waypoint4 waypoint1)
                      (anon-rover0-8 rover0 waypoint4)
                      (at_lander general waypoint1)
                      (anon-rover0-1 rover0 waypoint4))
    :effect (and (communicated_soil_data waypoint4)))

  (:action sample_soil---rover0---rover0---anon-rover0-18---waypoint4
   :parameters ()
   :precondition (and (anon-rover0-6 anon-rover0-18)
                      (anon-rover0-14 anon-rover0-18 rover0)
                      (anon-rover0-3 rover0) (at_soil_sample waypoint4)
                      (anon-rover0-1 rover0 waypoint4))
    :effect (and (not (anon-rover0-6 anon-rover0-18))
                 (not (at_soil_sample waypoint4))
                 (anon-rover0-8 rover0 waypoint4)
                 (anon-rover0-9 anon-rover0-18)))

  (:action communicate_rock_data---rover0---rover0---general---waypoint4---waypoint4---waypoint1
   :parameters ()
   :precondition (and (channel_free general) (anon-rover0-12 rover0)
                      (visible waypoint4 waypoint1)
                      (anon-rover0-7 rover0 waypoint4)
                      (at_lander general waypoint1)
                      (anon-rover0-1 rover0 waypoint4))
    :effect (and (communicated_rock_data waypoint4)))

  (:action drop---rover0---rover0---anon-rover0-18
   :parameters ()
   :precondition (and (anon-rover0-9 anon-rover0-18)
                      (anon-rover0-14 anon-rover0-18 rover0))
    :effect (and (not (anon-rover0-9 anon-rover0-18))
                 (anon-rover0-6 anon-rover0-18)))

  (:action sample_rock---rover0---rover0---anon-rover0-18---waypoint4
   :parameters ()
   :precondition (and (anon-rover0-6 anon-rover0-18)
                      (anon-rover0-14 anon-rover0-18 rover0)
                      (anon-rover0-4 rover0) (at_rock_sample waypoint4)
                      (anon-rover0-1 rover0 waypoint4))
    :effect (and (not (anon-rover0-6 anon-rover0-18))
                 (not (at_rock_sample waypoint4))
                 (anon-rover0-7 rover0 waypoint4)
                 (anon-rover0-9 anon-rover0-18)))

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