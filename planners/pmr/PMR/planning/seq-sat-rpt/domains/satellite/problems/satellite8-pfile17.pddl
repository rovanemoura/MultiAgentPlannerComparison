(define (problem pfile17) (:domain satellite)
  (:objects phenomenon24 phenomenon23 star22 star21 phenomenon20 phenomenon19 star18 phenomenon17 planet16 phenomenon15 phenomenon14 planet13 planet12 planet11 star10 phenomenon9 star8 planet7 planet6 planet5 star3 groundstation0 star1 groundstation4 groundstation2 - direction
             infrared2 image4 thermograph1 image0 infrared3 - mode
             instrument17 instrument16 - instrument
             satellite8 - satellite
            )
  (:init
       (supports instrument16 image0)
       (supports instrument16 infrared2)
       (calibration_target instrument16 star1)
       (supports instrument17 infrared3)
       (calibration_target instrument17 groundstation0)
       (on_board instrument16 satellite8)
       (on_board instrument17 satellite8)
       (power_avail satellite8)
       (pointing satellite8 phenomenon23))
  (:goal (and 
       (have_image phenomenon24 infrared3)
       (have_image phenomenon23 infrared3)
       (have_image planet16 infrared2)
       (have_image phenomenon15 infrared2)
       (have_image phenomenon14 infrared2)
       (have_image planet13 infrared3)
       (have_image planet5 image0)
       (pointing satellite8 planet16))))