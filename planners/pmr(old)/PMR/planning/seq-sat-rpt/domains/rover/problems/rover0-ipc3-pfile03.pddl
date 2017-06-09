(define (problem ipc3-pfile03) (:domain rover)
  (:objects objective0 objective1 - objective
             camera0 - camera
             waypoint0 waypoint1 waypoint2 waypoint3 - waypoint
             rover0store - store
             rover0 - rover
             colour high_res low_res - mode
             general - lander
            )
  (:init
       (visible waypoint0 waypoint1)
       (visible waypoint1 waypoint0)
       (visible waypoint0 waypoint3)
       (visible waypoint3 waypoint0)
       (visible waypoint1 waypoint2)
       (visible waypoint2 waypoint1)
       (visible waypoint1 waypoint3)
       (visible waypoint3 waypoint1)
       (visible waypoint2 waypoint0)
       (visible waypoint0 waypoint2)
       (visible waypoint3 waypoint2)
       (visible waypoint2 waypoint3)
       (at_rock_sample waypoint0)
       (at_rock_sample waypoint1)
       (at_soil_sample waypoint2)
       (at_rock_sample waypoint2)
       (at_lander general waypoint0)
       (channel_free general)
       (at rover0 waypoint1)
       (available rover0)
       (store_of rover0store rover0)
       (empty rover0store)
       (equipped_for_soil_analysis rover0)
       (equipped_for_rock_analysis rover0)
       (equipped_for_imaging rover0)
       (can_traverse rover0 waypoint1 waypoint0)
       (can_traverse rover0 waypoint0 waypoint1)
       (can_traverse rover0 waypoint1 waypoint3)
       (can_traverse rover0 waypoint3 waypoint1)
       (on_board camera0 rover0)
       (calibration_target camera0 objective1)
       (supports camera0 low_res)
       (visible_from objective0 waypoint0)
       (visible_from objective0 waypoint1)
       (visible_from objective1 waypoint0)
       (visible_from objective1 waypoint1))
  (:goal (and 
       (communicated_rock_data waypoint0))))