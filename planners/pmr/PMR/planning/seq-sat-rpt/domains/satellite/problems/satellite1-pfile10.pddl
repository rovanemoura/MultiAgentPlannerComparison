(define (problem pfile10) (:domain satellite)
  (:objects star16 star15 phenomenon14 phenomenon13 star12 star11 planet10 planet9 phenomenon8 star7 star6 planet5 star0 star2 star4 groundstation3 star1 - direction
             image2 image4 infrared3 spectrograph1 infrared0 - mode
             instrument3 instrument2 - instrument
             satellite1 - satellite
            )
  (:init
       (supports instrument2 infrared0)
       (supports instrument2 image2)
       (calibration_target instrument2 groundstation3)
       (supports instrument3 infrared3)
       (supports instrument3 infrared0)
       (calibration_target instrument3 star4)
       (on_board instrument2 satellite1)
       (on_board instrument3 satellite1)
       (power_avail satellite1)
       (pointing satellite1 star4))
  (:goal (and 
       (have_image star16 image2)
       (have_image planet10 infrared3)
       (have_image planet9 infrared0)
       (have_image star6 infrared3))))