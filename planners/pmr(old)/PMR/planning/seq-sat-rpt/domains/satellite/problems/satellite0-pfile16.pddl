(define (problem pfile16) (:domain satellite)
  (:objects star24 phenomenon23 planet22 planet21 star20 planet19 phenomenon18 phenomenon17 star16 star15 star14 star13 star12 planet11 planet10 phenomenon9 planet8 planet7 planet6 phenomenon5 groundstation1 star2 groundstation4 star3 groundstation0 - direction
             spectrograph3 thermograph1 infrared4 image2 image0 - mode
             instrument2 instrument1 instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 infrared4)
       (calibration_target instrument0 star3)
       (supports instrument1 spectrograph3)
       (calibration_target instrument1 groundstation0)
       (supports instrument2 image0)
       (supports instrument2 thermograph1)
       (supports instrument2 image2)
       (calibration_target instrument2 groundstation1)
       (on_board instrument0 satellite0)
       (on_board instrument1 satellite0)
       (on_board instrument2 satellite0)
       (power_avail satellite0)
       (pointing satellite0 star15))
  (:goal (and 
       (have_image star24 infrared4)
       (have_image phenomenon23 image0)
       (have_image planet22 image2)
       (have_image planet21 thermograph1)
       (have_image star20 image0)
       (have_image phenomenon18 spectrograph3)
       (have_image phenomenon17 infrared4)
       (have_image star16 thermograph1)
       (have_image star15 image0)
       (have_image star14 thermograph1)
       (have_image star13 image0)
       (have_image star12 image0)
       (have_image planet11 infrared4)
       (have_image planet10 image0)
       (have_image phenomenon9 image2)
       (have_image planet8 thermograph1)
       (have_image planet7 image0)
       (have_image planet6 infrared4)
       (have_image phenomenon5 thermograph1))))