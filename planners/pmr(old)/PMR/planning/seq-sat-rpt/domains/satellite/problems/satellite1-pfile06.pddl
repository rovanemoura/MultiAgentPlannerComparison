(define (problem pfile06) (:domain satellite)
  (:objects star10 star9 phenomenon8 star7 star6 planet5 planet4 star0 star2 star1 groundstation3 - direction
             infrared3 infrared1 spectrograph0 thermograph2 - mode
             instrument3 instrument2 instrument1 - instrument
             satellite1 - satellite
            )
  (:init
       (supports instrument1 infrared3)
       (calibration_target instrument1 star2)
       (supports instrument2 infrared1)
       (supports instrument2 infrared3)
       (supports instrument2 thermograph2)
       (calibration_target instrument2 star2)
       (supports instrument3 infrared1)
       (supports instrument3 infrared3)
       (supports instrument3 spectrograph0)
       (calibration_target instrument3 star2)
       (on_board instrument1 satellite1)
       (on_board instrument2 satellite1)
       (on_board instrument3 satellite1)
       (power_avail satellite1)
       (pointing satellite1 star6))
  (:goal (and 
       (have_image star10 infrared3)
       (have_image star9 infrared1)
       (have_image phenomenon8 spectrograph0)
       (have_image star7 infrared3)
       (have_image star6 thermograph2)
       (have_image planet5 spectrograph0)
       (have_image planet4 thermograph2))))