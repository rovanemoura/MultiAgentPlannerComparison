(define (problem pfile06) (:domain satellite)
  (:objects star10 star9 phenomenon8 star7 star6 planet5 planet4 star0 star2 star1 groundstation3 - direction
             infrared3 infrared1 spectrograph0 thermograph2 - mode
             instrument4 - instrument
             satellite2 - satellite
            )
  (:init
       (supports instrument4 infrared3)
       (calibration_target instrument4 star0)
       (on_board instrument4 satellite2)
       (power_avail satellite2)
       (pointing satellite2 star6))
  (:goal (and 
       (have_image star10 infrared3)
       (have_image star7 infrared3))))