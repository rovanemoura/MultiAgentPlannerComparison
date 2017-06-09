(define (problem pfile14) (:domain satellite)
  (:objects star24 planet23 star22 planet21 star20 star19 phenomenon18 planet17 planet16 planet15 star14 phenomenon13 phenomenon12 phenomenon11 star10 star9 planet8 phenomenon7 phenomenon6 phenomenon5 groundstation1 groundstation0 groundstation2 groundstation4 groundstation3 - direction
             thermograph0 image2 thermograph3 image1 thermograph4 - mode
             instrument11 - instrument
             satellite5 - satellite
            )
  (:init
       (supports instrument11 thermograph0)
       (supports instrument11 image2)
       (calibration_target instrument11 groundstation1)
       (on_board instrument11 satellite5)
       (power_avail satellite5)
       (pointing satellite5 phenomenon11))
  (:goal (and 
       (have_image planet21 thermograph0)
       (have_image planet17 image2)
       (have_image planet15 image2)
       (have_image phenomenon12 thermograph0)
       (have_image star9 thermograph0)
       (have_image planet8 image2)
       (have_image phenomenon7 thermograph0)
       (pointing satellite5 planet17))))