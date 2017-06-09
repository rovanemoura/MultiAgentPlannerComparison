(define (problem pfile17) (:domain satellite)
  (:objects phenomenon24 phenomenon23 star22 star21 phenomenon20 phenomenon19 star18 phenomenon17 planet16 phenomenon15 phenomenon14 planet13 planet12 planet11 star10 phenomenon9 star8 planet7 planet6 planet5 star3 groundstation0 star1 groundstation4 groundstation2 - direction
             infrared2 image4 thermograph1 image0 infrared3 - mode
             instrument22 instrument21 instrument20 - instrument
             satellite10 - satellite
            )
  (:init
       (supports instrument20 infrared2)
       (calibration_target instrument20 star1)
       (supports instrument21 thermograph1)
       (supports instrument21 image0)
       (calibration_target instrument21 star1)
       (supports instrument22 thermograph1)
       (calibration_target instrument22 groundstation0)
       (on_board instrument20 satellite10)
       (on_board instrument21 satellite10)
       (on_board instrument22 satellite10)
       (power_avail satellite10)
       (pointing satellite10 star22))
  (:goal (and 
       (have_image star21 thermograph1)
       (have_image phenomenon17 thermograph1)
       (have_image planet16 infrared2)
       (have_image phenomenon15 infrared2)
       (have_image phenomenon14 infrared2)
       (have_image planet12 thermograph1)
       (have_image star10 thermograph1)
       (have_image planet5 image0))))