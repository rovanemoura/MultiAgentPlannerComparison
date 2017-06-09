(define (problem pfile18) (:domain satellite)
  (:objects phenomenon24 planet23 star22 phenomenon21 planet20 planet19 phenomenon18 phenomenon17 phenomenon16 star15 planet14 phenomenon13 phenomenon12 planet11 star10 star9 star8 planet7 planet6 phenomenon5 star3 star1 star0 star4 groundstation2 - direction
             thermograph4 thermograph2 thermograph0 thermograph3 image1 - mode
             instrument6 instrument5 instrument4 instrument3 - instrument
             satellite1 - satellite
            )
  (:init
       (supports instrument3 thermograph3)
       (calibration_target instrument3 star1)
       (supports instrument4 image1)
       (calibration_target instrument4 star1)
       (supports instrument5 thermograph3)
       (calibration_target instrument5 star3)
       (supports instrument6 thermograph2)
       (supports instrument6 thermograph0)
       (supports instrument6 image1)
       (calibration_target instrument6 star0)
       (on_board instrument3 satellite1)
       (on_board instrument4 satellite1)
       (on_board instrument5 satellite1)
       (on_board instrument6 satellite1)
       (power_avail satellite1)
       (pointing satellite1 phenomenon21))
  (:goal (and 
       (have_image star22 thermograph3)
       (have_image phenomenon21 image1)
       (have_image planet19 thermograph2)
       (have_image phenomenon18 image1)
       (have_image star15 thermograph2)
       (have_image phenomenon13 thermograph2)
       (have_image star10 image1)
       (have_image star9 image1)
       (have_image star8 thermograph3)
       (have_image planet7 image1))))