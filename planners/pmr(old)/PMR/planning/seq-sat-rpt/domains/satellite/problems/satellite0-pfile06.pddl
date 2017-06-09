(define (problem pfile06) (:domain satellite)
  (:objects star10 star9 phenomenon8 star7 star6 planet5 planet4 star0 star2 star1 groundstation3 - direction
             infrared3 infrared1 spectrograph0 thermograph2 - mode
             instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 infrared1)
       (supports instrument0 spectrograph0)
       (calibration_target instrument0 star1)
       (on_board instrument0 satellite0)
       (power_avail satellite0)
       (pointing satellite0 phenomenon8))
  (:goal (and 
       (have_image star9 infrared1)
       (have_image phenomenon8 spectrograph0)
       (have_image planet5 spectrograph0))))