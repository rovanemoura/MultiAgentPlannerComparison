(define (problem pfile17) (:domain satellite)
  (:objects phenomenon24 phenomenon23 star22 star21 phenomenon20 phenomenon19 star18 phenomenon17 planet16 phenomenon15 phenomenon14 planet13 planet12 planet11 star10 phenomenon9 star8 planet7 planet6 planet5 star3 groundstation0 star1 groundstation4 groundstation2 - direction
             infrared2 image4 thermograph1 image0 infrared3 - mode
             instrument23 - instrument
             satellite11 - satellite
            )
  (:init
       (supports instrument23 infrared2)
       (supports instrument23 image4)
       (supports instrument23 thermograph1)
       (calibration_target instrument23 star3)
       (on_board instrument23 satellite11)
       (power_avail satellite11)
       (pointing satellite11 star8))
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