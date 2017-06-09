(define (problem pfile15) (:domain satellite)
  (:objects planet24 planet23 planet22 planet21 planet20 star19 star18 planet17 phenomenon16 star15 planet14 star13 star12 planet11 phenomenon10 phenomenon9 star8 planet7 planet6 phenomenon5 star4 star1 groundstation2 groundstation0 star3 - direction
             thermograph4 spectrograph2 thermograph3 infrared0 image1 - mode
             instrument18 instrument17 instrument16 - instrument
             satellite7 - satellite
            )
  (:init
       (supports instrument16 thermograph4)
       (calibration_target instrument16 groundstation2)
       (supports instrument17 spectrograph2)
       (calibration_target instrument17 star1)
       (supports instrument18 thermograph4)
       (calibration_target instrument18 star4)
       (on_board instrument16 satellite7)
       (on_board instrument17 satellite7)
       (on_board instrument18 satellite7)
       (power_avail satellite7)
       (pointing satellite7 planet11))
  (:goal (and 
       (have_image star15 thermograph4)
       (have_image planet14 thermograph4)
       (have_image planet6 spectrograph2)
       (have_image phenomenon5 spectrograph2)
       (pointing satellite7 star3))))