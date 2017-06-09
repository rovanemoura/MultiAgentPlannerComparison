(define (problem pfile11) (:domain satellite)
  (:objects planet19 star18 star17 planet16 phenomenon15 planet14 planet13 star12 star11 star10 phenomenon9 star8 phenomenon7 planet6 star5 star2 groundstation3 star0 star4 star1 - direction
             infrared0 spectrograph4 infrared1 image3 thermograph2 - mode
             instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 spectrograph4)
       (calibration_target instrument0 star0)
       (on_board instrument0 satellite0)
       (power_avail satellite0)
       (pointing satellite0 star8))
  (:goal (and 
       (have_image planet13 spectrograph4)
       (pointing satellite0 phenomenon9))))