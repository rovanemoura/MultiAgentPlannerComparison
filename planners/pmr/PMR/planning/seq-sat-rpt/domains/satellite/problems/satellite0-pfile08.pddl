(define (problem pfile08) (:domain satellite)
  (:objects phenomenon14 phenomenon13 phenomenon12 planet11 star10 phenomenon9 phenomenon8 star7 star6 phenomenon5 star4 star3 star0 groundstation1 star2 - direction
             spectrograph3 thermograph1 image0 thermograph2 - mode
             instrument2 instrument1 instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 thermograph1)
       (supports instrument0 image0)
       (calibration_target instrument0 star3)
       (supports instrument1 spectrograph3)
       (supports instrument1 thermograph2)
       (supports instrument1 thermograph1)
       (calibration_target instrument1 star2)
       (supports instrument2 spectrograph3)
       (calibration_target instrument2 star4)
       (on_board instrument0 satellite0)
       (on_board instrument1 satellite0)
       (on_board instrument2 satellite0)
       (power_avail satellite0)
       (pointing satellite0 phenomenon14))
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