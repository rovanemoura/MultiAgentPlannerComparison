(define (problem pfile09) (:domain satellite)
  (:objects star14 phenomenon13 phenomenon12 planet11 planet10 star9 phenomenon8 phenomenon7 phenomenon6 planet5 star2 star0 groundstation1 star3 star4 - direction
             image2 infrared1 image4 image3 spectrograph0 - mode
             instrument7 - instrument
             satellite3 - satellite
            )
  (:init
       (supports instrument7 image3)
       (supports instrument7 spectrograph0)
       (supports instrument7 image4)
       (calibration_target instrument7 star0)
       (on_board instrument7 satellite3)
       (power_avail satellite3)
       (pointing satellite3 planet10))
  (:goal (and 
       (have_image star14 image4)
       (have_image phenomenon13 spectrograph0)
       (have_image phenomenon12 image3)
       (have_image planet11 spectrograph0)
       (have_image planet10 image4)
       (have_image star9 image3)
       (have_image phenomenon6 image3)
       (pointing satellite3 star9))))