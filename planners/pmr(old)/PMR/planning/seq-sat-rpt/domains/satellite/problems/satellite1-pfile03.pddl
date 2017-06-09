(define (problem pfile03) (:domain satellite)
  (:objects phenomenon7 phenomenon6 phenomenon5 star4 star3 star0 star2 star1 - direction
             spectrograph2 infrared0 image1 - mode
             instrument3 - instrument
             satellite1 - satellite
            )
  (:init
       (supports instrument3 spectrograph2)
       (supports instrument3 infrared0)
       (supports instrument3 image1)
       (calibration_target instrument3 star0)
       (on_board instrument3 satellite1)
       (power_avail satellite1)
       (pointing satellite1 star0))
  (:goal (and 
       (have_image phenomenon7 spectrograph2)
       (have_image phenomenon5 spectrograph2)
       (have_image star4 spectrograph2)
       (have_image star3 infrared0))))