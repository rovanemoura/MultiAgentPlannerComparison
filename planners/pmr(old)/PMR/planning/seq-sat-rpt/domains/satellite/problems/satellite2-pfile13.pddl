(define (problem pfile13) (:domain satellite)
  (:objects planet29 planet28 phenomenon27 star26 planet25 planet24 planet23 star22 star21 planet20 star19 phenomenon18 planet17 planet16 planet15 star14 planet13 phenomenon12 planet11 planet10 planet9 planet8 planet7 planet6 phenomenon5 groundstation3 star0 star4 star1 groundstation2 - direction
             image3 thermograph2 thermograph0 thermograph1 image4 - mode
             instrument4 instrument3 - instrument
             satellite2 - satellite
            )
  (:init
       (supports instrument3 image4)
       (supports instrument3 image3)
       (calibration_target instrument3 star1)
       (supports instrument4 image3)
       (calibration_target instrument4 groundstation3)
       (on_board instrument3 satellite2)
       (on_board instrument4 satellite2)
       (power_avail satellite2)
       (pointing satellite2 planet7))
  (:goal (and 
       (have_image star22 image4)
       (have_image phenomenon18 image3)
       (have_image planet17 image4)
       (have_image planet16 image3)
       (have_image star14 image3)
       (have_image phenomenon12 image3)
       (have_image planet8 image3)
       (have_image planet7 image3)
       (have_image planet6 image4)
       (pointing satellite2 planet11))))