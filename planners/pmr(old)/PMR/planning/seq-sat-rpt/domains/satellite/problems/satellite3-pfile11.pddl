(define (problem pfile11) (:domain satellite)
  (:objects planet19 star18 star17 planet16 phenomenon15 planet14 planet13 star12 star11 star10 phenomenon9 star8 phenomenon7 planet6 star5 star2 groundstation3 star0 star4 star1 - direction
             infrared0 spectrograph4 infrared1 image3 thermograph2 - mode
             instrument7 - instrument
             satellite3 - satellite
            )
  (:init
       (supports instrument7 image3)
       (calibration_target instrument7 star2)
       (on_board instrument7 satellite3)
       (power_avail satellite3)
       (pointing satellite3 phenomenon9))
  (:goal (and 
       (have_image planet16 image3)
       (have_image star8 image3)
       (have_image star5 image3))))