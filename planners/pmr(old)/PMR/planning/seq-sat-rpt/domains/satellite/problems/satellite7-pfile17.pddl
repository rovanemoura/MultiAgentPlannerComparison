(define (problem pfile17) (:domain satellite)
  (:objects phenomenon24 phenomenon23 star22 star21 phenomenon20 phenomenon19 star18 phenomenon17 planet16 phenomenon15 phenomenon14 planet13 planet12 planet11 star10 phenomenon9 star8 planet7 planet6 planet5 star3 groundstation0 star1 groundstation4 groundstation2 - direction
             infrared2 image4 thermograph1 image0 infrared3 - mode
             instrument15 - instrument
             satellite7 - satellite
            )
  (:init
       (supports instrument15 image0)
       (supports instrument15 thermograph1)
       (calibration_target instrument15 groundstation4)
       (on_board instrument15 satellite7)
       (power_avail satellite7)
       (pointing satellite7 planet12))
  (:goal (and 
       (have_image star21 thermograph1)
       (have_image phenomenon17 thermograph1)
       (have_image planet12 thermograph1)
       (have_image star10 thermograph1)
       (have_image planet5 image0))))