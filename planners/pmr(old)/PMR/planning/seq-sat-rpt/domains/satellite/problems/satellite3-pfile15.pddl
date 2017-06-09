(define (problem pfile15) (:domain satellite)
  (:objects planet24 planet23 planet22 planet21 planet20 star19 star18 planet17 phenomenon16 star15 planet14 star13 star12 planet11 phenomenon10 phenomenon9 star8 planet7 planet6 phenomenon5 star4 star1 groundstation2 groundstation0 star3 - direction
             thermograph4 spectrograph2 thermograph3 infrared0 image1 - mode
             instrument6 - instrument
             satellite3 - satellite
            )
  (:init
       (supports instrument6 spectrograph2)
       (supports instrument6 infrared0)
       (calibration_target instrument6 groundstation2)
       (on_board instrument6 satellite3)
       (power_avail satellite3)
       (pointing satellite3 star4))
  (:goal (and 
       (have_image planet24 infrared0)
       (have_image planet21 infrared0)
       (have_image phenomenon9 infrared0)
       (have_image planet7 infrared0)
       (have_image planet6 spectrograph2)
       (have_image phenomenon5 spectrograph2)
       (pointing satellite3 planet14))))