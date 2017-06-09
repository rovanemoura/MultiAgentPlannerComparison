(define (problem pfile15) (:domain satellite)
  (:objects planet24 planet23 planet22 planet21 planet20 star19 star18 planet17 phenomenon16 star15 planet14 star13 star12 planet11 phenomenon10 phenomenon9 star8 planet7 planet6 phenomenon5 star4 star1 groundstation2 groundstation0 star3 - direction
             thermograph4 spectrograph2 thermograph3 infrared0 image1 - mode
             instrument12 instrument11 instrument10 - instrument
             satellite5 - satellite
            )
  (:init
       (supports instrument10 thermograph4)
       (supports instrument10 spectrograph2)
       (supports instrument10 infrared0)
       (calibration_target instrument10 groundstation0)
       (supports instrument11 infrared0)
       (calibration_target instrument11 groundstation0)
       (supports instrument12 infrared0)
       (calibration_target instrument12 star1)
       (on_board instrument10 satellite5)
       (on_board instrument11 satellite5)
       (on_board instrument12 satellite5)
       (power_avail satellite5)
       (pointing satellite5 planet6))
  (:goal (and 
       (have_image planet24 infrared0)
       (have_image planet21 infrared0)
       (have_image star15 thermograph4)
       (have_image planet14 thermograph4)
       (have_image phenomenon9 infrared0)
       (have_image planet7 infrared0)
       (have_image planet6 spectrograph2)
       (have_image phenomenon5 spectrograph2)
       (pointing satellite5 planet24))))