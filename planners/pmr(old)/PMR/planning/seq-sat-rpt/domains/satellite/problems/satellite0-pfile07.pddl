(define (problem pfile07) (:domain satellite)
  (:objects planet11 planet10 planet9 planet8 star7 star6 phenomenon5 groundstation0 groundstation4 star1 groundstation2 star3 - direction
             image3 image0 image1 image2 - mode
             instrument2 instrument1 instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 image1)
       (supports instrument0 image3)
       (calibration_target instrument0 star1)
       (supports instrument1 image3)
       (calibration_target instrument1 groundstation0)
       (supports instrument2 image0)
       (calibration_target instrument2 groundstation2)
       (on_board instrument0 satellite0)
       (on_board instrument1 satellite0)
       (on_board instrument2 satellite0)
       (power_avail satellite0)
       (pointing satellite0 star6))
  (:goal (and 
       (have_image planet10 image0)
       (have_image planet9 image3)
       (have_image planet8 image0)
       (have_image star7 image0)
       (have_image star6 image1)
       (have_image phenomenon5 image0))))