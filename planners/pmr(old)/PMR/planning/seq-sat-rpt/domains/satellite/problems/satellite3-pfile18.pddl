(define (problem pfile18) (:domain satellite)
  (:objects phenomenon24 planet23 star22 phenomenon21 planet20 planet19 phenomenon18 phenomenon17 phenomenon16 star15 planet14 phenomenon13 phenomenon12 planet11 star10 star9 star8 planet7 planet6 phenomenon5 star3 star1 star0 star4 groundstation2 - direction
             thermograph4 thermograph2 thermograph0 thermograph3 image1 - mode
             instrument11 instrument10 - instrument
             satellite3 - satellite
            )
  (:init
       (supports instrument10 thermograph2)
       (calibration_target instrument10 star3)
       (supports instrument11 thermograph2)
       (supports instrument11 thermograph4)
       (supports instrument11 thermograph0)
       (calibration_target instrument11 star1)
       (on_board instrument10 satellite3)
       (on_board instrument11 satellite3)
       (power_avail satellite3)
       (pointing satellite3 phenomenon16))
  (:goal (and 
       (have_image planet20 thermograph4)
       (have_image planet19 thermograph2)
       (have_image phenomenon17 thermograph4)
       (have_image star15 thermograph2)
       (have_image phenomenon13 thermograph2)
       (have_image phenomenon5 thermograph4))))