(define (problem pfile16) (:domain satellite)
  (:objects star24 phenomenon23 planet22 planet21 star20 planet19 phenomenon18 phenomenon17 star16 star15 star14 star13 star12 planet11 planet10 phenomenon9 planet8 planet7 planet6 phenomenon5 groundstation1 star2 groundstation4 star3 groundstation0 - direction
             spectrograph3 thermograph1 infrared4 image2 image0 - mode
             instrument22 - instrument
             satellite9 - satellite
            )
  (:init
       (supports instrument22 spectrograph3)
       (supports instrument22 thermograph1)
       (supports instrument22 infrared4)
       (calibration_target instrument22 groundstation1)
       (on_board instrument22 satellite9)
       (power_avail satellite9)
       (pointing satellite9 planet11))
  (:goal (and 
       (have_image star24 infrared4)
       (have_image planet21 thermograph1)
       (have_image phenomenon18 spectrograph3)
       (have_image phenomenon17 infrared4)
       (have_image star16 thermograph1)
       (have_image star14 thermograph1)
       (have_image planet11 infrared4)
       (have_image planet8 thermograph1)
       (have_image planet6 infrared4)
       (have_image phenomenon5 thermograph1)
       (pointing satellite9 star16))))