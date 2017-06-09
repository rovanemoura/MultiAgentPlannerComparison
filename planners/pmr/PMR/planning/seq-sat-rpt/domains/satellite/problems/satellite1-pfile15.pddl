(define (problem pfile15) (:domain satellite)
  (:objects planet24 planet23 planet22 planet21 planet20 star19 star18 planet17 phenomenon16 star15 planet14 star13 star12 planet11 phenomenon10 phenomenon9 star8 planet7 planet6 phenomenon5 star4 star1 groundstation2 groundstation0 star3 - direction
             thermograph4 spectrograph2 thermograph3 infrared0 image1 - mode
             instrument3 instrument2 - instrument
             satellite1 - satellite
            )
  (:init
       (supports instrument2 spectrograph2)
       (calibration_target instrument2 star4)
       (supports instrument3 image1)
       (supports instrument3 spectrograph2)
       (calibration_target instrument3 groundstation2)
       (on_board instrument2 satellite1)
       (on_board instrument3 satellite1)
       (power_avail satellite1)
       (pointing satellite1 star18))
  (:goal (and 
       (have_image planet22 image1)
       (have_image planet20 image1)
       (have_image star18 image1)
       (have_image phenomenon16 image1)
       (have_image planet11 image1)
       (have_image phenomenon10 image1)
       (have_image planet6 spectrograph2)
       (have_image phenomenon5 spectrograph2)
       (pointing satellite1 planet22))))