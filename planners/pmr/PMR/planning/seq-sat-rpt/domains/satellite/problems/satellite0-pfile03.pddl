(define (problem pfile03) (:domain satellite)
  (:objects phenomenon7 phenomenon6 phenomenon5 star4 star3 star0 star2 star1 - direction
             spectrograph2 infrared0 image1 - mode
             instrument2 instrument1 instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 spectrograph2)
       (supports instrument0 infrared0)
       (calibration_target instrument0 star1)
       (supports instrument1 image1)
       (calibration_target instrument1 star2)
       (supports instrument2 infrared0)
       (supports instrument2 image1)
       (calibration_target instrument2 star0)
       (on_board instrument0 satellite0)
       (on_board instrument1 satellite0)
       (on_board instrument2 satellite0)
       (power_avail satellite0)
       (pointing satellite0 star4))
  (:goal (and 
       (have_image phenomenon7 spectrograph2)
       (have_image phenomenon5 spectrograph2)
       (have_image star4 spectrograph2)
       (have_image star3 infrared0)
       (pointing satellite0 phenomenon5))))