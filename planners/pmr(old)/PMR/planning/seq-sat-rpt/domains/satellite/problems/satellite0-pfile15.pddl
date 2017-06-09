(define (problem pfile15) (:domain satellite)
  (:objects planet24 planet23 planet22 planet21 planet20 star19 star18 planet17 phenomenon16 star15 planet14 star13 star12 planet11 phenomenon10 phenomenon9 star8 planet7 planet6 phenomenon5 star4 star1 groundstation2 groundstation0 star3 - direction
             thermograph4 spectrograph2 thermograph3 infrared0 image1 - mode
             instrument1 instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 thermograph4)
       (supports instrument0 image1)
       (calibration_target instrument0 groundstation0)
       (supports instrument1 spectrograph2)
       (supports instrument1 thermograph3)
       (calibration_target instrument1 star3)
       (on_board instrument0 satellite0)
       (on_board instrument1 satellite0)
       (power_avail satellite0)
       (pointing satellite0 star19))
  (:goal (and 
       (have_image planet23 thermograph3)
       (have_image planet22 image1)
       (have_image planet20 image1)
       (have_image star18 image1)
       (have_image planet17 thermograph3)
       (have_image phenomenon16 image1)
       (have_image star15 thermograph4)
       (have_image planet14 thermograph4)
       (have_image star13 thermograph3)
       (have_image star12 thermograph3)
       (have_image planet11 image1)
       (have_image phenomenon10 image1)
       (have_image planet6 spectrograph2)
       (have_image phenomenon5 spectrograph2)
       (pointing satellite0 star19))))