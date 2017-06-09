(define (problem pfile05) (:domain satellite)
  (:objects planet9 phenomenon8 star7 phenomenon6 phenomenon5 star4 star3 groundstation0 groundstation1 groundstation2 - direction
             spectrograph1 image2 thermograph0 - mode
             instrument5 instrument4 instrument3 - instrument
             satellite1 - satellite
            )
  (:init
       (supports instrument3 spectrograph1)
       (supports instrument3 thermograph0)
       (calibration_target instrument3 groundstation0)
       (supports instrument4 image2)
       (supports instrument4 spectrograph1)
       (calibration_target instrument4 groundstation2)
       (supports instrument5 image2)
       (supports instrument5 spectrograph1)
       (supports instrument5 thermograph0)
       (calibration_target instrument5 groundstation1)
       (on_board instrument3 satellite1)
       (on_board instrument4 satellite1)
       (on_board instrument5 satellite1)
       (power_avail satellite1)
       (pointing satellite1 groundstation2))
  (:goal (and 
       (have_image planet9 spectrograph1)
       (have_image phenomenon8 image2)
       (have_image star7 thermograph0)
       (have_image phenomenon6 image2)
       (have_image phenomenon5 image2)
       (have_image star3 thermograph0)
       (pointing satellite1 groundstation2))))