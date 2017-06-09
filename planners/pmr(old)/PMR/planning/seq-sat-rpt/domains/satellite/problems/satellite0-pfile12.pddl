(define (problem pfile12) (:domain satellite)
  (:objects phenomenon24 star23 star22 phenomenon21 planet20 star19 star18 phenomenon17 planet16 star15 star14 phenomenon13 star12 planet11 planet10 star9 planet8 star7 phenomenon6 planet5 star4 star2 groundstation1 star3 star0 - direction
             infrared3 spectrograph4 infrared1 infrared0 thermograph2 - mode
             instrument2 instrument1 instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 infrared1)
       (supports instrument0 spectrograph4)
       (calibration_target instrument0 star0)
       (supports instrument1 infrared1)
       (supports instrument1 infrared0)
       (calibration_target instrument1 star2)
       (supports instrument2 infrared1)
       (supports instrument2 infrared0)
       (calibration_target instrument2 star3)
       (on_board instrument0 satellite0)
       (on_board instrument1 satellite0)
       (on_board instrument2 satellite0)
       (power_avail satellite0)
       (pointing satellite0 planet16))
  (:goal (and 
       (have_image phenomenon24 infrared0)
       (have_image star23 spectrograph4)
       (have_image star22 infrared1)
       (have_image star18 spectrograph4)
       (have_image phenomenon17 spectrograph4)
       (have_image planet16 infrared1)
       (have_image phenomenon13 spectrograph4)
       (have_image star9 spectrograph4)
       (have_image planet8 infrared1)
       (have_image star7 infrared0)
       (have_image phenomenon6 spectrograph4)
       (have_image planet5 infrared0))))