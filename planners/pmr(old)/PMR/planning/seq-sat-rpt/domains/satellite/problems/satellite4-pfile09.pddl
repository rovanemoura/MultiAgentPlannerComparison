(define (problem pfile09) (:domain satellite)
  (:objects star14 phenomenon13 phenomenon12 planet11 planet10 star9 phenomenon8 phenomenon7 phenomenon6 planet5 star2 star0 groundstation1 star3 star4 - direction
             image2 infrared1 image4 image3 spectrograph0 - mode
             instrument10 instrument9 instrument8 - instrument
             satellite4 - satellite
            )
  (:init
       (supports instrument8 image4)
       (supports instrument8 infrared1)
       (supports instrument8 image3)
       (calibration_target instrument8 groundstation1)
       (supports instrument9 image4)
       (calibration_target instrument9 star0)
       (supports instrument10 image2)
       (supports instrument10 infrared1)
       (supports instrument10 image4)
       (calibration_target instrument10 star2)
       (on_board instrument8 satellite4)
       (on_board instrument9 satellite4)
       (on_board instrument10 satellite4)
       (power_avail satellite4)
       (pointing satellite4 star9))
  (:goal (and 
       (have_image star14 image4)
       (have_image phenomenon12 image3)
       (have_image planet10 image4)
       (have_image star9 image3)
       (have_image phenomenon8 image2)
       (have_image phenomenon7 infrared1)
       (have_image phenomenon6 image3)
       (have_image planet5 image2)
       (pointing satellite4 planet5))))