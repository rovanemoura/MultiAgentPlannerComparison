(define (problem pfile09) (:domain satellite)
  (:objects star14 phenomenon13 phenomenon12 planet11 planet10 star9 phenomenon8 phenomenon7 phenomenon6 planet5 star2 star0 groundstation1 star3 star4 - direction
             image2 infrared1 image4 image3 spectrograph0 - mode
             instrument6 - instrument
             satellite2 - satellite
            )
  (:init
       (supports instrument6 image2)
       (supports instrument6 spectrograph0)
       (calibration_target instrument6 star2)
       (on_board instrument6 satellite2)
       (power_avail satellite2)
       (pointing satellite2 phenomenon6))
  (:goal (and 
       (have_image phenomenon13 spectrograph0)
       (have_image planet11 spectrograph0)
       (have_image phenomenon8 image2)
       (have_image planet5 image2))))