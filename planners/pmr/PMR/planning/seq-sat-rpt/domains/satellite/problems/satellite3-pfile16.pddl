(define (problem pfile16) (:domain satellite)
  (:objects star24 phenomenon23 planet22 planet21 star20 planet19 phenomenon18 phenomenon17 star16 star15 star14 star13 star12 planet11 planet10 phenomenon9 planet8 planet7 planet6 phenomenon5 groundstation1 star2 groundstation4 star3 groundstation0 - direction
             spectrograph3 thermograph1 infrared4 image2 image0 - mode
             instrument7 - instrument
             satellite3 - satellite
            )
  (:init
       (supports instrument7 infrared4)
       (calibration_target instrument7 star3)
       (on_board instrument7 satellite3)
       (power_avail satellite3)
       (pointing satellite3 phenomenon9))
  (:goal (and 
       (have_image star24 infrared4)
       (have_image phenomenon17 infrared4)
       (have_image planet11 infrared4)
       (have_image planet6 infrared4))))