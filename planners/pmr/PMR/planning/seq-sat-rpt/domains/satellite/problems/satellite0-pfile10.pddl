(define (problem pfile10) (:domain satellite)
  (:objects star16 star15 phenomenon14 phenomenon13 star12 star11 planet10 planet9 phenomenon8 star7 star6 planet5 star0 star2 star4 groundstation3 star1 - direction
             image2 image4 infrared3 spectrograph1 infrared0 - mode
             instrument1 instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 image4)
       (calibration_target instrument0 star1)
       (supports instrument1 infrared0)
       (supports instrument1 spectrograph1)
       (calibration_target instrument1 groundstation3)
       (on_board instrument0 satellite0)
       (on_board instrument1 satellite0)
       (power_avail satellite0)
       (pointing satellite0 star0))
  (:goal (and 
       (have_image star15 spectrograph1)
       (have_image phenomenon14 spectrograph1)
       (have_image phenomenon13 image4)
       (have_image star12 image4)
       (have_image planet9 infrared0)
       (have_image phenomenon8 image4)
       (have_image star7 image4)
       (have_image planet5 image4))))