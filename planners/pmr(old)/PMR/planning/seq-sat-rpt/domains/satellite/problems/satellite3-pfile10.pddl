(define (problem pfile10) (:domain satellite)
  (:objects star16 star15 phenomenon14 phenomenon13 star12 star11 planet10 planet9 phenomenon8 star7 star6 planet5 star0 star2 star4 groundstation3 star1 - direction
             image2 image4 infrared3 spectrograph1 infrared0 - mode
             instrument7 instrument6 - instrument
             satellite3 - satellite
            )
  (:init
       (supports instrument6 infrared0)
       (supports instrument6 infrared3)
       (calibration_target instrument6 groundstation3)
       (supports instrument7 image4)
       (supports instrument7 spectrograph1)
       (supports instrument7 infrared3)
       (calibration_target instrument7 star4)
       (on_board instrument6 satellite3)
       (on_board instrument7 satellite3)
       (power_avail satellite3)
       (pointing satellite3 groundstation3))
  (:goal (and 
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