(define (problem pfile18) (:domain satellite)
  (:objects phenomenon24 planet23 star22 phenomenon21 planet20 planet19 phenomenon18 phenomenon17 phenomenon16 star15 planet14 phenomenon13 phenomenon12 planet11 star10 star9 star8 planet7 planet6 phenomenon5 star3 star1 star0 star4 groundstation2 - direction
             thermograph4 thermograph2 thermograph0 thermograph3 image1 - mode
             instrument9 instrument8 instrument7 - instrument
             satellite2 - satellite
            )
  (:init
       (supports instrument7 thermograph0)
       (calibration_target instrument7 star3)
       (supports instrument8 thermograph4)
       (supports instrument8 thermograph3)
       (supports instrument8 thermograph2)
       (calibration_target instrument8 star3)
       (supports instrument9 thermograph2)
       (supports instrument9 thermograph3)
       (calibration_target instrument9 star1)
       (on_board instrument7 satellite2)
       (on_board instrument8 satellite2)
       (on_board instrument9 satellite2)
       (power_avail satellite2)
       (pointing satellite2 star4))
  (:goal (and 
       (have_image star22 thermograph3)
       (have_image planet20 thermograph4)
       (have_image planet19 thermograph2)
       (have_image phenomenon17 thermograph4)
       (have_image star15 thermograph2)
       (have_image phenomenon13 thermograph2)
       (have_image star8 thermograph3)
       (have_image phenomenon5 thermograph4))))