(define (problem pfile17) (:domain satellite)
  (:objects phenomenon24 phenomenon23 star22 star21 phenomenon20 phenomenon19 star18 phenomenon17 planet16 phenomenon15 phenomenon14 planet13 planet12 planet11 star10 phenomenon9 star8 planet7 planet6 planet5 star3 groundstation0 star1 groundstation4 groundstation2 - direction
             infrared2 image4 thermograph1 image0 infrared3 - mode
             instrument2 instrument1 - instrument
             satellite1 - satellite
            )
  (:init
       (supports instrument1 image0)
       (supports instrument1 infrared2)
       (calibration_target instrument1 star3)
       (supports instrument2 thermograph1)
       (supports instrument2 image0)
       (calibration_target instrument2 groundstation0)
       (on_board instrument1 satellite1)
       (on_board instrument2 satellite1)
       (power_avail satellite1)
       (pointing satellite1 planet5))
  (:goal (and 
       (have_image star21 thermograph1)
       (have_image phenomenon17 thermograph1)
       (have_image planet16 infrared2)
       (have_image phenomenon15 infrared2)
       (have_image phenomenon14 infrared2)
       (have_image planet12 thermograph1)
       (have_image star10 thermograph1)
       (have_image planet5 image0)
       (pointing satellite1 star22))))