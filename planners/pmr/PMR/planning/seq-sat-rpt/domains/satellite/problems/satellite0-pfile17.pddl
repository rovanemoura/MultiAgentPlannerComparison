(define (problem pfile17) (:domain satellite)
  (:objects phenomenon24 phenomenon23 star22 star21 phenomenon20 phenomenon19 star18 phenomenon17 planet16 phenomenon15 phenomenon14 planet13 planet12 planet11 star10 phenomenon9 star8 planet7 planet6 planet5 star3 groundstation0 star1 groundstation4 groundstation2 - direction
             infrared2 image4 thermograph1 image0 infrared3 - mode
             instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 infrared3)
       (calibration_target instrument0 groundstation0)
       (on_board instrument0 satellite0)
       (power_avail satellite0)
       (pointing satellite0 planet5))
  (:goal (and 
       (have_image phenomenon24 infrared3)
       (have_image phenomenon23 infrared3)
       (have_image planet13 infrared3))))