(define (problem pfile08) (:domain satellite)
  (:objects phenomenon14 phenomenon13 phenomenon12 planet11 star10 phenomenon9 phenomenon8 star7 star6 phenomenon5 star4 star3 star0 groundstation1 star2 - direction
             spectrograph3 thermograph1 image0 thermograph2 - mode
             instrument9 instrument8 - instrument
             satellite3 - satellite
            )
  (:init
       (supports instrument8 image0)
       (calibration_target instrument8 star3)
       (supports instrument9 spectrograph3)
       (supports instrument9 thermograph1)
       (supports instrument9 image0)
       (calibration_target instrument9 star4)
       (on_board instrument8 satellite3)
       (on_board instrument9 satellite3)
       (power_avail satellite3)
       (pointing satellite3 phenomenon5))
  (:goal (and 
       (have_image phenomenon13 thermograph1)
       (have_image phenomenon12 image0)
       (have_image star10 spectrograph3)
       (have_image phenomenon9 image0)
       (have_image phenomenon8 image0)
       (have_image star7 spectrograph3)
       (have_image star6 thermograph1)
       (have_image phenomenon5 thermograph1))))