(define (problem pfile08) (:domain satellite)
  (:objects phenomenon14 phenomenon13 phenomenon12 planet11 star10 phenomenon9 phenomenon8 star7 star6 phenomenon5 star4 star3 star0 groundstation1 star2 - direction
             spectrograph3 thermograph1 image0 thermograph2 - mode
             instrument5 instrument4 instrument3 - instrument
             satellite1 - satellite
            )
  (:init
       (supports instrument3 thermograph2)
       (supports instrument3 image0)
       (calibration_target instrument3 groundstation1)
       (supports instrument4 thermograph1)
       (calibration_target instrument4 star4)
       (supports instrument5 thermograph2)
       (supports instrument5 thermograph1)
       (supports instrument5 spectrograph3)
       (calibration_target instrument5 star0)
       (on_board instrument3 satellite1)
       (on_board instrument4 satellite1)
       (on_board instrument5 satellite1)
       (power_avail satellite1)
       (pointing satellite1 star4))
  (:goal (and 
       (have_image phenomenon14 thermograph2)
       (have_image phenomenon13 thermograph1)
       (have_image phenomenon12 image0)
       (have_image planet11 thermograph2)
       (have_image star10 spectrograph3)
       (have_image phenomenon9 image0)
       (have_image phenomenon8 image0)
       (have_image star7 spectrograph3)
       (have_image star6 thermograph1)
       (have_image phenomenon5 thermograph1))))