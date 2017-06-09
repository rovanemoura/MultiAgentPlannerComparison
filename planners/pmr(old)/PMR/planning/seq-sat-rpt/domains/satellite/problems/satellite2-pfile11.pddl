(define (problem pfile11) (:domain satellite)
  (:objects planet19 star18 star17 planet16 phenomenon15 planet14 planet13 star12 star11 star10 phenomenon9 star8 phenomenon7 planet6 star5 star2 groundstation3 star0 star4 star1 - direction
             infrared0 spectrograph4 infrared1 image3 thermograph2 - mode
             instrument6 instrument5 instrument4 - instrument
             satellite2 - satellite
            )
  (:init
       (supports instrument4 infrared1)
       (supports instrument4 image3)
       (supports instrument4 infrared0)
       (calibration_target instrument4 star2)
       (supports instrument5 thermograph2)
       (supports instrument5 spectrograph4)
       (calibration_target instrument5 star0)
       (supports instrument6 infrared0)
       (calibration_target instrument6 groundstation3)
       (on_board instrument4 satellite2)
       (on_board instrument5 satellite2)
       (on_board instrument6 satellite2)
       (power_avail satellite2)
       (pointing satellite2 star4))
  (:goal (and 
       (have_image star17 infrared0)
       (have_image planet16 image3)
       (have_image phenomenon15 infrared0)
       (have_image planet14 thermograph2)
       (have_image planet13 spectrograph4)
       (have_image star11 infrared1)
       (have_image star10 thermograph2)
       (have_image star8 image3)
       (have_image phenomenon7 infrared1)
       (have_image planet6 infrared1)
       (have_image star5 image3))))