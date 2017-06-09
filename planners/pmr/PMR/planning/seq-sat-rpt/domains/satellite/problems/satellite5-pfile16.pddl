(define (problem pfile16) (:domain satellite)
  (:objects star24 phenomenon23 planet22 planet21 star20 planet19 phenomenon18 phenomenon17 star16 star15 star14 star13 star12 planet11 planet10 phenomenon9 planet8 planet7 planet6 phenomenon5 groundstation1 star2 groundstation4 star3 groundstation0 - direction
             spectrograph3 thermograph1 infrared4 image2 image0 - mode
             instrument13 instrument12 instrument11 - instrument
             satellite5 - satellite
            )
  (:init
       (supports instrument11 image0)
       (calibration_target instrument11 star3)
       (supports instrument12 infrared4)
       (supports instrument12 image0)
       (calibration_target instrument12 groundstation4)
       (supports instrument13 spectrograph3)
       (calibration_target instrument13 star2)
       (on_board instrument11 satellite5)
       (on_board instrument12 satellite5)
       (on_board instrument13 satellite5)
       (power_avail satellite5)
       (pointing satellite5 planet10))
  (:goal (and 
       (have_image star24 infrared4)
       (have_image phenomenon23 image0)
       (have_image star20 image0)
       (have_image phenomenon18 spectrograph3)
       (have_image phenomenon17 infrared4)
       (have_image star15 image0)
       (have_image star13 image0)
       (have_image star12 image0)
       (have_image planet11 infrared4)
       (have_image planet10 image0)
       (have_image planet7 image0)
       (have_image planet6 infrared4)
       (pointing satellite5 planet6))))