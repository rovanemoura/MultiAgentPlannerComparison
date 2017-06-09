(define (problem pfile15) (:domain satellite)
  (:objects planet24 planet23 planet22 planet21 planet20 star19 star18 planet17 phenomenon16 star15 planet14 star13 star12 planet11 phenomenon10 phenomenon9 star8 planet7 planet6 phenomenon5 star4 star1 groundstation2 groundstation0 star3 - direction
             thermograph4 spectrograph2 thermograph3 infrared0 image1 - mode
             instrument15 instrument14 instrument13 - instrument
             satellite6 - satellite
            )
  (:init
       (supports instrument13 thermograph3)
       (supports instrument13 infrared0)
       (calibration_target instrument13 star3)
       (supports instrument14 spectrograph2)
       (calibration_target instrument14 groundstation2)
       (supports instrument15 thermograph4)
       (calibration_target instrument15 groundstation0)
       (on_board instrument13 satellite6)
       (on_board instrument14 satellite6)
       (on_board instrument15 satellite6)
       (power_avail satellite6)
       (pointing satellite6 planet17))
  (:goal (and 
       (have_image planet24 infrared0)
       (have_image planet23 thermograph3)
       (have_image planet21 infrared0)
       (have_image planet17 thermograph3)
       (have_image star15 thermograph4)
       (have_image planet14 thermograph4)
       (have_image star13 thermograph3)
       (have_image star12 thermograph3)
       (have_image phenomenon9 infrared0)
       (have_image planet7 infrared0)
       (have_image planet6 spectrograph2)
       (have_image phenomenon5 spectrograph2))))