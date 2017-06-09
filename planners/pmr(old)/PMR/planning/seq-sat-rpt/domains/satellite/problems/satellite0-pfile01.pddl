(define (problem pfile01) (:domain satellite)
  (:objects phenomenon6 star5 phenomenon4 phenomenon3 groundstation2 groundstation1 star0 - direction
             thermograph0 spectrograph2 image1 - mode
             instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 thermograph0)
       (calibration_target instrument0 groundstation2)
       (on_board instrument0 satellite0)
       (power_avail satellite0)
       (pointing satellite0 phenomenon6))
  (:goal (and 
       (have_image phenomenon6 thermograph0)
       (have_image star5 thermograph0)
       (have_image phenomenon4 thermograph0))))