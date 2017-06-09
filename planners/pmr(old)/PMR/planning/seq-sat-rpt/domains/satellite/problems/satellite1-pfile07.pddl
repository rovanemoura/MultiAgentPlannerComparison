(define (problem pfile07) (:domain satellite)
  (:objects planet11 planet10 planet9 planet8 star7 star6 phenomenon5 groundstation0 groundstation4 star1 groundstation2 star3 - direction
             image3 image0 image1 image2 - mode
             instrument3 - instrument
             satellite1 - satellite
            )
  (:init
       (supports instrument3 image0)
       (supports instrument3 image2)
       (calibration_target instrument3 groundstation4)
       (on_board instrument3 satellite1)
       (power_avail satellite1)
       (pointing satellite1 groundstation0))
  (:goal (and 
       (have_image planet11 image2)
       (have_image planet10 image0)
       (have_image planet8 image0)
       (have_image star7 image0)
       (have_image phenomenon5 image0)
       (pointing satellite1 star1))))