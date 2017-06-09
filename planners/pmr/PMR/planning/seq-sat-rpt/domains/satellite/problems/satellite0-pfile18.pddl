(define (problem pfile18) (:domain satellite)
  (:objects phenomenon24 planet23 star22 phenomenon21 planet20 planet19 phenomenon18 phenomenon17 phenomenon16 star15 planet14 phenomenon13 phenomenon12 planet11 star10 star9 star8 planet7 planet6 phenomenon5 star3 star1 star0 star4 groundstation2 - direction
             thermograph4 thermograph2 thermograph0 thermograph3 image1 - mode
             instrument2 instrument1 instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 thermograph4)
       (supports instrument0 thermograph0)
       (supports instrument0 thermograph2)
       (calibration_target instrument0 star4)
       (supports instrument1 thermograph3)
       (calibration_target instrument1 star0)
       (supports instrument2 image1)
       (calibration_target instrument2 star4)
       (on_board instrument0 satellite0)
       (on_board instrument1 satellite0)
       (on_board instrument2 satellite0)
       (power_avail satellite0)
       (pointing satellite0 star8))
  (:goal (and 
       (have_image star22 thermograph3)
       (have_image phenomenon21 image1)
       (have_image planet20 thermograph4)
       (have_image planet19 thermograph2)
       (have_image phenomenon18 image1)
       (have_image phenomenon17 thermograph4)
       (have_image star15 thermograph2)
       (have_image phenomenon13 thermograph2)
       (have_image star10 image1)
       (have_image star9 image1)
       (have_image star8 thermograph3)
       (have_image planet7 image1)
       (have_image phenomenon5 thermograph4))))