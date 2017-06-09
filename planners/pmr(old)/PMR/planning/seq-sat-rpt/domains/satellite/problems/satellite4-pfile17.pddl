(define (problem pfile17) (:domain satellite)
  (:objects phenomenon24 phenomenon23 star22 star21 phenomenon20 phenomenon19 star18 phenomenon17 planet16 phenomenon15 phenomenon14 planet13 planet12 planet11 star10 phenomenon9 star8 planet7 planet6 planet5 star3 groundstation0 star1 groundstation4 groundstation2 - direction
             infrared2 image4 thermograph1 image0 infrared3 - mode
             instrument10 instrument9 - instrument
             satellite4 - satellite
            )
  (:init
       (supports instrument9 infrared3)
       (calibration_target instrument9 star1)
       (supports instrument10 image4)
       (supports instrument10 image0)
       (calibration_target instrument10 star3)
       (on_board instrument9 satellite4)
       (on_board instrument10 satellite4)
       (power_avail satellite4)
       (pointing satellite4 star22))
  (:goal (and 
       (have_image phenomenon24 infrared3)
       (have_image phenomenon23 infrared3)
       (have_image star22 image4)
       (have_image star18 image4)
       (have_image planet13 infrared3)
       (have_image planet11 image4)
       (have_image phenomenon9 image4)
       (have_image planet7 image4)
       (have_image planet6 image4)
       (have_image planet5 image0)
       (pointing satellite4 phenomenon20))))