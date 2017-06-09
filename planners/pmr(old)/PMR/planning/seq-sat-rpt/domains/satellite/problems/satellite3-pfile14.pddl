(define (problem pfile14) (:domain satellite)
  (:objects star24 planet23 star22 planet21 star20 star19 phenomenon18 planet17 planet16 planet15 star14 phenomenon13 phenomenon12 phenomenon11 star10 star9 planet8 phenomenon7 phenomenon6 phenomenon5 groundstation1 groundstation0 groundstation2 groundstation4 groundstation3 - direction
             thermograph0 image2 thermograph3 image1 thermograph4 - mode
             instrument8 instrument7 - instrument
             satellite3 - satellite
            )
  (:init
       (supports instrument7 image2)
       (supports instrument7 thermograph3)
       (calibration_target instrument7 groundstation4)
       (supports instrument8 thermograph4)
       (supports instrument8 thermograph0)
       (calibration_target instrument8 groundstation2)
       (on_board instrument7 satellite3)
       (on_board instrument8 satellite3)
       (power_avail satellite3)
       (pointing satellite3 groundstation4))
  (:goal (and 
       (have_image star22 thermograph3)
       (have_image planet21 thermograph0)
       (have_image star20 thermograph4)
       (have_image star19 thermograph4)
       (have_image planet17 image2)
       (have_image planet15 image2)
       (have_image star14 thermograph4)
       (have_image phenomenon12 thermograph0)
       (have_image star10 thermograph3)
       (have_image star9 thermograph0)
       (have_image planet8 image2)
       (have_image phenomenon7 thermograph0))))