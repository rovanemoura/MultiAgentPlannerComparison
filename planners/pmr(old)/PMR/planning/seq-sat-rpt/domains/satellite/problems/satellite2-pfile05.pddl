(define (problem pfile05) (:domain satellite)
  (:objects planet9 phenomenon8 star7 phenomenon6 phenomenon5 star4 star3 groundstation0 groundstation1 groundstation2 - direction
             spectrograph1 image2 thermograph0 - mode
             instrument8 instrument7 instrument6 - instrument
             satellite2 - satellite
            )
  (:init
       (supports instrument6 image2)
       (calibration_target instrument6 groundstation1)
       (supports instrument7 image2)
       (supports instrument7 thermograph0)
       (calibration_target instrument7 groundstation1)
       (supports instrument8 spectrograph1)
       (supports instrument8 image2)
       (supports instrument8 thermograph0)
       (calibration_target instrument8 groundstation0)
       (on_board instrument6 satellite2)
       (on_board instrument7 satellite2)
       (on_board instrument8 satellite2)
       (power_avail satellite2)
       (pointing satellite2 phenomenon5))
  (:goal (and 
       (have_image planet9 spectrograph1)
       (have_image phenomenon8 image2)
       (have_image star7 thermograph0)
       (have_image phenomenon6 image2)
       (have_image phenomenon5 image2)
       (have_image star3 thermograph0))))