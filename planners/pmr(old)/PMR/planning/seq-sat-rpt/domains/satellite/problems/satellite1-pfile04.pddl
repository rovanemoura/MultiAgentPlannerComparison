(define (problem pfile04) (:domain satellite)
  (:objects phenomenon9 phenomenon8 star7 star6 planet5 star4 planet3 star2 star0 groundstation1 - direction
             thermograph2 infrared1 infrared0 - mode
             instrument2 instrument1 - instrument
             satellite1 - satellite
            )
  (:init
       (supports instrument1 infrared0)
       (supports instrument1 thermograph2)
       (supports instrument1 infrared1)
       (calibration_target instrument1 star2)
       (supports instrument2 thermograph2)
       (supports instrument2 infrared1)
       (calibration_target instrument2 star2)
       (on_board instrument1 satellite1)
       (on_board instrument2 satellite1)
       (power_avail satellite1)
       (pointing satellite1 star0))
  (:goal (and 
       (have_image phenomenon9 infrared0)
       (have_image phenomenon8 thermograph2)
       (have_image star7 infrared0)
       (have_image star6 infrared1)
       (have_image planet5 thermograph2)
       (have_image star4 infrared1)
       (have_image planet3 infrared1)
       (pointing satellite1 planet5))))