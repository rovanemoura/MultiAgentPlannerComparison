(define (problem pfile10) (:domain satellite)
  (:objects star16 star15 phenomenon14 phenomenon13 star12 star11 planet10 planet9 phenomenon8 star7 star6 planet5 star0 star2 star4 groundstation3 star1 - direction
             image2 image4 infrared3 spectrograph1 infrared0 - mode
             instrument5 instrument4 - instrument
             satellite2 - satellite
            )
  (:init
       (supports instrument4 spectrograph1)
       (supports instrument4 image4)
       (supports instrument4 infrared0)
       (calibration_target instrument4 star2)
       (supports instrument5 image2)
       (supports instrument5 infrared0)
       (supports instrument5 infrared3)
       (calibration_target instrument5 star0)
       (on_board instrument4 satellite2)
       (on_board instrument5 satellite2)
       (power_avail satellite2)
       (pointing satellite2 star1))
  (:goal (and 
       (have_image star16 image2)
       (have_image star15 spectrograph1)
       (have_image phenomenon14 spectrograph1)
       (have_image phenomenon13 image4)
       (have_image star12 image4)
       (have_image planet10 infrared3)
       (have_image planet9 infrared0)
       (have_image phenomenon8 image4)
       (have_image star7 image4)
       (have_image star6 infrared3)
       (have_image planet5 image4))))