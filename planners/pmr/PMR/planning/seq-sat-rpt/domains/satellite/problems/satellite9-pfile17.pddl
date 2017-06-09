(define (problem pfile17) (:domain satellite)
  (:objects phenomenon24 phenomenon23 star22 star21 phenomenon20 phenomenon19 star18 phenomenon17 planet16 phenomenon15 phenomenon14 planet13 planet12 planet11 star10 phenomenon9 star8 planet7 planet6 planet5 star3 groundstation0 star1 groundstation4 groundstation2 - direction
             infrared2 image4 thermograph1 image0 infrared3 - mode
             instrument19 instrument18 - instrument
             satellite9 - satellite
            )
  (:init
       (supports instrument18 thermograph1)
       (supports instrument18 infrared2)
       (supports instrument18 image4)
       (calibration_target instrument18 star1)
       (supports instrument19 infrared3)
       (calibration_target instrument19 groundstation4)
       (on_board instrument18 satellite9)
       (on_board instrument19 satellite9)
       (power_avail satellite9)
       (pointing satellite9 phenomenon20))
  (:goal (and 
       (have_image phenomenon24 infrared3)
       (have_image phenomenon23 infrared3)
       (have_image star22 image4)
       (have_image star21 thermograph1)
       (have_image star18 image4)
       (have_image phenomenon17 thermograph1)
       (have_image planet16 infrared2)
       (have_image phenomenon15 infrared2)
       (have_image phenomenon14 infrared2)
       (have_image planet13 infrared3)
       (have_image planet12 thermograph1)
       (have_image planet11 image4)
       (have_image star10 thermograph1)
       (have_image phenomenon9 image4)
       (have_image planet7 image4)
       (have_image planet6 image4))))