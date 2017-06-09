(define (problem pfile07) (:domain satellite)
  (:objects planet11 planet10 planet9 planet8 star7 star6 phenomenon5 groundstation0 groundstation4 star1 groundstation2 star3 - direction
             image3 image0 image1 image2 - mode
             instrument7 instrument6 - instrument
             satellite3 - satellite
            )
  (:init
       (supports instrument6 image2)
       (supports instrument6 image1)
       (supports instrument6 image0)
       (calibration_target instrument6 groundstation4)
       (supports instrument7 image3)
       (supports instrument7 image0)
       (supports instrument7 image1)
       (calibration_target instrument7 groundstation0)
       (on_board instrument6 satellite3)
       (on_board instrument7 satellite3)
       (power_avail satellite3)
       (pointing satellite3 groundstation2))
  (:goal (and 
       (have_image planet11 image2)
       (have_image planet10 image0)
       (have_image planet9 image3)
       (have_image planet8 image0)
       (have_image star7 image0)
       (have_image star6 image1)
       (have_image phenomenon5 image0))))