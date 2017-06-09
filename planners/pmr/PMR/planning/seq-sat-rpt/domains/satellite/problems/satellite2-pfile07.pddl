(define (problem pfile07) (:domain satellite)
  (:objects planet11 planet10 planet9 planet8 star7 star6 phenomenon5 groundstation0 groundstation4 star1 groundstation2 star3 - direction
             image3 image0 image1 image2 - mode
             instrument5 instrument4 - instrument
             satellite2 - satellite
            )
  (:init
       (supports instrument4 image1)
       (supports instrument4 image0)
       (calibration_target instrument4 star1)
       (supports instrument5 image2)
       (supports instrument5 image0)
       (supports instrument5 image1)
       (calibration_target instrument5 star1)
       (on_board instrument4 satellite2)
       (on_board instrument5 satellite2)
       (power_avail satellite2)
       (pointing satellite2 star6))
  (:goal (and 
       (have_image planet11 image2)
       (have_image planet10 image0)
       (have_image planet8 image0)
       (have_image star7 image0)
       (have_image star6 image1)
       (have_image phenomenon5 image0)
       (pointing satellite2 phenomenon5))))