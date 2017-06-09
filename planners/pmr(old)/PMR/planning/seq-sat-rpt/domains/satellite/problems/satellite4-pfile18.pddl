(define (problem pfile18) (:domain satellite)
  (:objects phenomenon24 planet23 star22 phenomenon21 planet20 planet19 phenomenon18 phenomenon17 phenomenon16 star15 planet14 phenomenon13 phenomenon12 planet11 star10 star9 star8 planet7 planet6 phenomenon5 star3 star1 star0 star4 groundstation2 - direction
             thermograph4 thermograph2 thermograph0 thermograph3 image1 - mode
             instrument12 - instrument
             satellite4 - satellite
            )
  (:init
       (supports instrument12 thermograph4)
       (calibration_target instrument12 star3)
       (on_board instrument12 satellite4)
       (power_avail satellite4)
       (pointing satellite4 phenomenon18))
  (:goal (and 
       (have_image planet20 thermograph4)
       (have_image phenomenon17 thermograph4)
       (have_image phenomenon5 thermograph4))))