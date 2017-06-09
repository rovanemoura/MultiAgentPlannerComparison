(define (problem pfile11) (:domain satellite)
  (:objects planet19 star18 star17 planet16 phenomenon15 planet14 planet13 star12 star11 star10 phenomenon9 star8 phenomenon7 planet6 star5 star2 groundstation3 star0 star4 star1 - direction
             infrared0 spectrograph4 infrared1 image3 thermograph2 - mode
             instrument8 - instrument
             satellite4 - satellite
            )
  (:init
       (supports instrument8 infrared0)
       (supports instrument8 spectrograph4)
       (supports instrument8 infrared1)
       (calibration_target instrument8 star2)
       (on_board instrument8 satellite4)
       (power_avail satellite4)
       (pointing satellite4 phenomenon9))
  (:goal (and 
       (have_image star17 infrared0)
       (have_image phenomenon15 infrared0)
       (have_image planet13 spectrograph4)
       (have_image star11 infrared1)
       (have_image phenomenon7 infrared1)
       (have_image planet6 infrared1)
       (pointing satellite4 star11))))