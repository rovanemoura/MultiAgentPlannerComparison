(define (problem pfile16) (:domain satellite)
  (:objects star24 phenomenon23 planet22 planet21 star20 planet19 phenomenon18 phenomenon17 star16 star15 star14 star13 star12 planet11 planet10 phenomenon9 planet8 planet7 planet6 phenomenon5 groundstation1 star2 groundstation4 star3 groundstation0 - direction
             spectrograph3 thermograph1 infrared4 image2 image0 - mode
             instrument18 instrument17 - instrument
             satellite7 - satellite
            )
  (:init
       (supports instrument17 thermograph1)
       (supports instrument17 image2)
       (supports instrument17 image0)
       (calibration_target instrument17 groundstation4)
       (supports instrument18 image2)
       (supports instrument18 thermograph1)
       (calibration_target instrument18 star3)
       (on_board instrument17 satellite7)
       (on_board instrument18 satellite7)
       (power_avail satellite7)
       (pointing satellite7 planet11))
  (:goal (and 
       (have_image phenomenon23 image0)
       (have_image planet22 image2)
       (have_image planet21 thermograph1)
       (have_image star20 image0)
       (have_image star16 thermograph1)
       (have_image star15 image0)
       (have_image star14 thermograph1)
       (have_image star13 image0)
       (have_image star12 image0)
       (have_image planet10 image0)
       (have_image phenomenon9 image2)
       (have_image planet8 thermograph1)
       (have_image planet7 image0)
       (have_image phenomenon5 thermograph1)
       (pointing satellite7 star3))))