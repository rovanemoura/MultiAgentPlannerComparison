(define (problem pfile08) (:domain satellite)
  (:objects phenomenon14 phenomenon13 phenomenon12 planet11 star10 phenomenon9 phenomenon8 star7 star6 phenomenon5 star4 star3 star0 groundstation1 star2 - direction
             spectrograph3 thermograph1 image0 thermograph2 - mode
             instrument7 instrument6 - instrument
             satellite2 - satellite
            )
  (:init
       (supports instrument6 thermograph1)
       (supports instrument6 thermograph2)
       (calibration_target instrument6 star3)
       (supports instrument7 thermograph2)
       (supports instrument7 thermograph1)
       (supports instrument7 image0)
       (calibration_target instrument7 star0)
       (on_board instrument6 satellite2)
       (on_board instrument7 satellite2)
       (power_avail satellite2)
       (pointing satellite2 star6))
  (:goal (and 
       (have_image phenomenon14 thermograph2)
       (have_image phenomenon13 thermograph1)
       (have_image phenomenon12 image0)
       (have_image planet11 thermograph2)
       (have_image phenomenon9 image0)
       (have_image phenomenon8 image0)
       (have_image star6 thermograph1)
       (have_image phenomenon5 thermograph1))))