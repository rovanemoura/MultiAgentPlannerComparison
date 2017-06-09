(define (problem pfile14) (:domain satellite)
  (:objects star24 planet23 star22 planet21 star20 star19 phenomenon18 planet17 planet16 planet15 star14 phenomenon13 phenomenon12 phenomenon11 star10 star9 planet8 phenomenon7 phenomenon6 phenomenon5 groundstation1 groundstation0 groundstation2 groundstation4 groundstation3 - direction
             thermograph0 image2 thermograph3 image1 thermograph4 - mode
             instrument3 - instrument
             satellite1 - satellite
            )
  (:init
       (supports instrument3 thermograph0)
       (supports instrument3 thermograph4)
       (supports instrument3 image2)
       (calibration_target instrument3 groundstation2)
       (on_board instrument3 satellite1)
       (power_avail satellite1)
       (pointing satellite1 groundstation1))
  (:goal (and 
       (have_image planet21 thermograph0)
       (have_image star20 thermograph4)
       (have_image star19 thermograph4)
       (have_image planet17 image2)
       (have_image planet15 image2)
       (have_image star14 thermograph4)
       (have_image phenomenon12 thermograph0)
       (have_image star9 thermograph0)
       (have_image planet8 image2)
       (have_image phenomenon7 thermograph0))))