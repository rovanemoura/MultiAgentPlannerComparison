(define (problem pfile09) (:domain satellite)
  (:objects star14 phenomenon13 phenomenon12 planet11 planet10 star9 phenomenon8 phenomenon7 phenomenon6 planet5 star2 star0 groundstation1 star3 star4 - direction
             image2 infrared1 image4 image3 spectrograph0 - mode
             instrument5 instrument4 instrument3 - instrument
             satellite1 - satellite
            )
  (:init
       (supports instrument3 image2)
       (supports instrument3 image3)
       (supports instrument3 image4)
       (calibration_target instrument3 star2)
       (supports instrument4 image3)
       (supports instrument4 image2)
       (calibration_target instrument4 star3)
       (supports instrument5 image4)
       (supports instrument5 infrared1)
       (supports instrument5 spectrograph0)
       (calibration_target instrument5 star3)
       (on_board instrument3 satellite1)
       (on_board instrument4 satellite1)
       (on_board instrument5 satellite1)
       (power_avail satellite1)
       (pointing satellite1 planet11))
  (:goal (and 
       (have_image star14 image4)
       (have_image phenomenon13 spectrograph0)
       (have_image phenomenon12 image3)
       (have_image planet11 spectrograph0)
       (have_image planet10 image4)
       (have_image star9 image3)
       (have_image phenomenon8 image2)
       (have_image phenomenon7 infrared1)
       (have_image phenomenon6 image3)
       (have_image planet5 image2))))