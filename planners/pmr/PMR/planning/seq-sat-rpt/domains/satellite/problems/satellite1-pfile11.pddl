(define (problem pfile11) (:domain satellite)
  (:objects planet19 star18 star17 planet16 phenomenon15 planet14 planet13 star12 star11 star10 phenomenon9 star8 phenomenon7 planet6 star5 star2 groundstation3 star0 star4 star1 - direction
             infrared0 spectrograph4 infrared1 image3 thermograph2 - mode
             instrument3 instrument2 instrument1 - instrument
             satellite1 - satellite
            )
  (:init
       (supports instrument1 infrared0)
       (supports instrument1 infrared1)
       (calibration_target instrument1 groundstation3)
       (supports instrument2 infrared1)
       (supports instrument2 infrared0)
       (calibration_target instrument2 star2)
       (supports instrument3 spectrograph4)
       (supports instrument3 infrared1)
       (supports instrument3 thermograph2)
       (calibration_target instrument3 star0)
       (on_board instrument1 satellite1)
       (on_board instrument2 satellite1)
       (on_board instrument3 satellite1)
       (power_avail satellite1)
       (pointing satellite1 groundstation3))
  (:goal (and 
       (have_image star17 infrared0)
       (have_image phenomenon15 infrared0)
       (have_image planet14 thermograph2)
       (have_image planet13 spectrograph4)
       (have_image star11 infrared1)
       (have_image star10 thermograph2)
       (have_image phenomenon7 infrared1)
       (have_image planet6 infrared1)
       (pointing satellite1 star4))))