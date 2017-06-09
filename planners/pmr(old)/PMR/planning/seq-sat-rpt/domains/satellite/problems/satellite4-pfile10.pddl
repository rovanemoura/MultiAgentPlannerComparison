(define (problem pfile10) (:domain satellite)
  (:objects star16 star15 phenomenon14 phenomenon13 star12 star11 planet10 planet9 phenomenon8 star7 star6 planet5 star0 star2 star4 groundstation3 star1 - direction
             image2 image4 infrared3 spectrograph1 infrared0 - mode
             instrument10 instrument9 instrument8 - instrument
             satellite4 - satellite
            )
  (:init
       (supports instrument8 spectrograph1)
       (supports instrument8 image4)
       (calibration_target instrument8 star4)
       (supports instrument9 infrared3)
       (calibration_target instrument9 star2)
       (supports instrument10 image2)
       (supports instrument10 image4)
       (calibration_target instrument10 star0)
       (on_board instrument8 satellite4)
       (on_board instrument9 satellite4)
       (on_board instrument10 satellite4)
       (power_avail satellite4)
       (pointing satellite4 planet10))
  (:goal (and 
       (have_image star16 image2)
       (have_image star15 spectrograph1)
       (have_image phenomenon14 spectrograph1)
       (have_image phenomenon13 image4)
       (have_image star12 image4)
       (have_image planet10 infrared3)
       (have_image phenomenon8 image4)
       (have_image star7 image4)
       (have_image star6 infrared3)
       (have_image planet5 image4)
       (pointing satellite4 planet9))))