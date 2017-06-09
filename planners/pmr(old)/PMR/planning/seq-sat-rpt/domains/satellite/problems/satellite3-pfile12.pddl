(define (problem pfile12) (:domain satellite)
  (:objects phenomenon24 star23 star22 phenomenon21 planet20 star19 star18 phenomenon17 planet16 star15 star14 phenomenon13 star12 planet11 planet10 star9 planet8 star7 phenomenon6 planet5 star4 star2 groundstation1 star3 star0 - direction
             infrared3 spectrograph4 infrared1 infrared0 thermograph2 - mode
             instrument6 - instrument
             satellite3 - satellite
            )
  (:init
       (supports instrument6 infrared1)
       (calibration_target instrument6 star4)
       (on_board instrument6 satellite3)
       (power_avail satellite3)
       (pointing satellite3 phenomenon6))
  (:goal (and 
       (have_image star22 infrared1)
       (have_image planet16 infrared1)
       (have_image planet8 infrared1))))