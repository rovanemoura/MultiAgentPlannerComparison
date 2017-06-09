(define (problem pfile16) (:domain satellite)
  (:objects star24 phenomenon23 planet22 planet21 star20 planet19 phenomenon18 phenomenon17 star16 star15 star14 star13 star12 planet11 planet10 phenomenon9 planet8 planet7 planet6 phenomenon5 groundstation1 star2 groundstation4 star3 groundstation0 - direction
             spectrograph3 thermograph1 infrared4 image2 image0 - mode
             instrument21 instrument20 instrument19 - instrument
             satellite8 - satellite
            )
  (:init
       (supports instrument19 thermograph1)
       (supports instrument19 infrared4)
       (calibration_target instrument19 star2)
       (supports instrument20 thermograph1)
       (calibration_target instrument20 groundstation4)
       (supports instrument21 thermograph1)
       (calibration_target instrument21 star2)
       (on_board instrument19 satellite8)
       (on_board instrument20 satellite8)
       (on_board instrument21 satellite8)
       (power_avail satellite8)
       (pointing satellite8 groundstation4))
  (:goal (and 
       (have_image star24 infrared4)
       (have_image planet21 thermograph1)
       (have_image phenomenon17 infrared4)
       (have_image star16 thermograph1)
       (have_image star14 thermograph1)
       (have_image planet11 infrared4)
       (have_image planet8 thermograph1)
       (have_image planet6 infrared4)
       (have_image phenomenon5 thermograph1)
       (pointing satellite8 star15))))