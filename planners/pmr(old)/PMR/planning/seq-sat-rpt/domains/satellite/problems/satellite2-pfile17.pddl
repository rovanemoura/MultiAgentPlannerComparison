(define (problem pfile17) (:domain satellite)
  (:objects phenomenon24 phenomenon23 star22 star21 phenomenon20 phenomenon19 star18 phenomenon17 planet16 phenomenon15 phenomenon14 planet13 planet12 planet11 star10 phenomenon9 star8 planet7 planet6 planet5 star3 groundstation0 star1 groundstation4 groundstation2 - direction
             infrared2 image4 thermograph1 image0 infrared3 - mode
             instrument5 instrument4 instrument3 - instrument
             satellite2 - satellite
            )
  (:init
       (supports instrument3 infrared3)
       (supports instrument3 infrared2)
       (calibration_target instrument3 groundstation4)
       (supports instrument4 infrared3)
       (supports instrument4 infrared2)
       (supports instrument4 thermograph1)
       (calibration_target instrument4 groundstation2)
       (supports instrument5 thermograph1)
       (calibration_target instrument5 groundstation4)
       (on_board instrument3 satellite2)
       (on_board instrument4 satellite2)
       (on_board instrument5 satellite2)
       (power_avail satellite2)
       (pointing satellite2 star21))
  (:goal (and 
       (have_image phenomenon24 infrared3)
       (have_image phenomenon23 infrared3)
       (have_image star21 thermograph1)
       (have_image phenomenon17 thermograph1)
       (have_image planet16 infrared2)
       (have_image phenomenon15 infrared2)
       (have_image phenomenon14 infrared2)
       (have_image planet13 infrared3)
       (have_image planet12 thermograph1)
       (have_image star10 thermograph1))))