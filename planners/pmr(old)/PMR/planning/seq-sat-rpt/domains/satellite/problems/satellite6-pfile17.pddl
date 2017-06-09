(define (problem pfile17) (:domain satellite)
  (:objects phenomenon24 phenomenon23 star22 star21 phenomenon20 phenomenon19 star18 phenomenon17 planet16 phenomenon15 phenomenon14 planet13 planet12 planet11 star10 phenomenon9 star8 planet7 planet6 planet5 star3 groundstation0 star1 groundstation4 groundstation2 - direction
             infrared2 image4 thermograph1 image0 infrared3 - mode
             instrument14 instrument13 instrument12 - instrument
             satellite6 - satellite
            )
  (:init
       (supports instrument12 image4)
       (calibration_target instrument12 groundstation0)
       (supports instrument13 image4)
       (calibration_target instrument13 star1)
       (supports instrument14 thermograph1)
       (supports instrument14 infrared2)
       (calibration_target instrument14 groundstation2)
       (on_board instrument12 satellite6)
       (on_board instrument13 satellite6)
       (on_board instrument14 satellite6)
       (power_avail satellite6)
       (pointing satellite6 phenomenon20))
  (:goal (and 
       (have_image star22 image4)
       (have_image star21 thermograph1)
       (have_image star18 image4)
       (have_image phenomenon17 thermograph1)
       (have_image planet16 infrared2)
       (have_image phenomenon15 infrared2)
       (have_image phenomenon14 infrared2)
       (have_image planet12 thermograph1)
       (have_image planet11 image4)
       (have_image star10 thermograph1)
       (have_image phenomenon9 image4)
       (have_image planet7 image4)
       (have_image planet6 image4))))