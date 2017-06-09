(define (problem pfile13) (:domain satellite)
  (:objects planet29 planet28 phenomenon27 star26 planet25 planet24 planet23 star22 star21 planet20 star19 phenomenon18 planet17 planet16 planet15 star14 planet13 phenomenon12 planet11 planet10 planet9 planet8 planet7 planet6 phenomenon5 groundstation3 star0 star4 star1 groundstation2 - direction
             image3 thermograph2 thermograph0 thermograph1 image4 - mode
             instrument1 instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 image4)
       (calibration_target instrument0 groundstation3)
       (supports instrument1 thermograph1)
       (supports instrument1 image4)
       (calibration_target instrument1 groundstation3)
       (on_board instrument0 satellite0)
       (on_board instrument1 satellite0)
       (power_avail satellite0)
       (pointing satellite0 star19))
  (:goal (and 
       (have_image phenomenon27 thermograph1)
       (have_image planet25 thermograph1)
       (have_image planet23 thermograph1)
       (have_image star22 image4)
       (have_image star21 thermograph1)
       (have_image planet17 image4)
       (have_image planet13 thermograph1)
       (have_image planet10 thermograph1)
       (have_image planet6 image4)
       (have_image phenomenon5 thermograph1))))