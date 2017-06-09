(define (problem pfile02) (:domain satellite)
  (:objects star7 phenomenon6 phenomenon5 planet4 planet3 groundstation2 star0 groundstation1 - direction
             image2 infrared1 infrared0 - mode
             instrument1 instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 infrared1)
       (supports instrument0 infrared0)
       (calibration_target instrument0 star0)
       (supports instrument1 image2)
       (supports instrument1 infrared1)
       (supports instrument1 infrared0)
       (calibration_target instrument1 groundstation2)
       (on_board instrument0 satellite0)
       (on_board instrument1 satellite0)
       (power_avail satellite0)
       (pointing satellite0 planet4))
  (:goal (and 
       (have_image star7 infrared0)
       (have_image phenomenon6 infrared0)
       (have_image phenomenon5 image2)
       (have_image planet4 infrared0)
       (have_image planet3 infrared0))))