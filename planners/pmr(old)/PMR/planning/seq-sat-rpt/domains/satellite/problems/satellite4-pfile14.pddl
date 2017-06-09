(define (problem pfile14) (:domain satellite)
  (:objects star24 planet23 star22 planet21 star20 star19 phenomenon18 planet17 planet16 planet15 star14 phenomenon13 phenomenon12 phenomenon11 star10 star9 planet8 phenomenon7 phenomenon6 phenomenon5 groundstation1 groundstation0 groundstation2 groundstation4 groundstation3 - direction
             thermograph0 image2 thermograph3 image1 thermograph4 - mode
             instrument10 instrument9 - instrument
             satellite4 - satellite
            )
  (:init
       (supports instrument9 thermograph0)
       (supports instrument9 image2)
       (supports instrument9 image1)
       (calibration_target instrument9 groundstation2)
       (supports instrument10 thermograph3)
       (supports instrument10 image1)
       (calibration_target instrument10 groundstation0)
       (on_board instrument9 satellite4)
       (on_board instrument10 satellite4)
       (power_avail satellite4)
       (pointing satellite4 planet15))
  (:goal (and 
       (have_image planet23 image1)
       (have_image star22 thermograph3)
       (have_image planet21 thermograph0)
       (have_image phenomenon18 image1)
       (have_image planet17 image2)
       (have_image planet15 image2)
       (have_image phenomenon13 image1)
       (have_image phenomenon12 thermograph0)
       (have_image star10 thermograph3)
       (have_image star9 thermograph0)
       (have_image planet8 image2)
       (have_image phenomenon7 thermograph0)
       (have_image phenomenon5 image1))))