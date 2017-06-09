(define (problem pfile09) (:domain satellite)
  (:objects star14 phenomenon13 phenomenon12 planet11 planet10 star9 phenomenon8 phenomenon7 phenomenon6 planet5 star2 star0 groundstation1 star3 star4 - direction
             image2 infrared1 image4 image3 spectrograph0 - mode
             instrument2 instrument1 instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 infrared1)
       (supports instrument0 image4)
       (calibration_target instrument0 star3)
       (supports instrument1 image4)
       (supports instrument1 image2)
       (supports instrument1 spectrograph0)
       (calibration_target instrument1 star4)
       (supports instrument2 image2)
       (calibration_target instrument2 star2)
       (on_board instrument0 satellite0)
       (on_board instrument1 satellite0)
       (on_board instrument2 satellite0)
       (power_avail satellite0)
       (pointing satellite0 star0))
  (:goal (and 
       (have_image star14 image4)
       (have_image phenomenon13 spectrograph0)
       (have_image planet11 spectrograph0)
       (have_image planet10 image4)
       (have_image phenomenon8 image2)
       (have_image phenomenon7 infrared1)
       (have_image planet5 image2)
       (pointing satellite0 phenomenon7))))