(define (problem pfile04) (:domain satellite)
  (:objects phenomenon9 phenomenon8 star7 star6 planet5 star4 planet3 star2 star0 groundstation1 - direction
             thermograph2 infrared1 infrared0 - mode
             instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 thermograph2)
       (supports instrument0 infrared0)
       (calibration_target instrument0 star0)
       (on_board instrument0 satellite0)
       (power_avail satellite0)
       (pointing satellite0 star6))
  (:goal (and 
       (have_image phenomenon9 infrared0)
       (have_image phenomenon8 thermograph2)
       (have_image star7 infrared0)
       (have_image planet5 thermograph2))))