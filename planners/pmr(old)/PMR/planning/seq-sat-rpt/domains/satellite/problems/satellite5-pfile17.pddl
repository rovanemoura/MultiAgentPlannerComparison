(define (problem pfile17) (:domain satellite)
  (:objects phenomenon24 phenomenon23 star22 star21 phenomenon20 phenomenon19 star18 phenomenon17 planet16 phenomenon15 phenomenon14 planet13 planet12 planet11 star10 phenomenon9 star8 planet7 planet6 planet5 star3 groundstation0 star1 groundstation4 groundstation2 - direction
             infrared2 image4 thermograph1 image0 infrared3 - mode
             instrument11 - instrument
             satellite5 - satellite
            )
  (:init
       (supports instrument11 infrared2)
       (calibration_target instrument11 star1)
       (on_board instrument11 satellite5)
       (power_avail satellite5)
       (pointing satellite5 groundstation2))
  (:goal (and 
       (have_image planet16 infrared2)
       (have_image phenomenon15 infrared2)
       (have_image phenomenon14 infrared2))))