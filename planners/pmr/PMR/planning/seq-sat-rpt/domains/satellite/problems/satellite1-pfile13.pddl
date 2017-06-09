(define (problem pfile13) (:domain satellite)
  (:objects planet29 planet28 phenomenon27 star26 planet25 planet24 planet23 star22 star21 planet20 star19 phenomenon18 planet17 planet16 planet15 star14 planet13 phenomenon12 planet11 planet10 planet9 planet8 planet7 planet6 phenomenon5 groundstation3 star0 star4 star1 groundstation2 - direction
             image3 thermograph2 thermograph0 thermograph1 image4 - mode
             instrument2 - instrument
             satellite1 - satellite
            )
  (:init
       (supports instrument2 thermograph0)
       (supports instrument2 image4)
       (supports instrument2 thermograph2)
       (calibration_target instrument2 groundstation3)
       (on_board instrument2 satellite1)
       (power_avail satellite1)
       (pointing satellite1 planet17))
  (:goal (and 
       (have_image planet29 thermograph0)
       (have_image planet28 thermograph2)
       (have_image star26 thermograph0)
       (have_image planet24 thermograph2)
       (have_image star22 image4)
       (have_image star19 thermograph0)
       (have_image planet17 image4)
       (have_image planet15 thermograph0)
       (have_image planet11 thermograph2)
       (have_image planet9 thermograph0)
       (have_image planet6 image4)
       (pointing satellite1 phenomenon5))))