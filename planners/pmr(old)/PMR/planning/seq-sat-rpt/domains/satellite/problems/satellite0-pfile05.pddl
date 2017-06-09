(define (problem pfile05) (:domain satellite)
  (:objects planet9 phenomenon8 star7 phenomenon6 phenomenon5 star4 star3 groundstation0 groundstation1 groundstation2 - direction
             spectrograph1 image2 thermograph0 - mode
             instrument2 instrument1 instrument0 - instrument
             satellite0 - satellite
            )
  (:init
       (supports instrument0 image2)
       (supports instrument0 thermograph0)
       (supports instrument0 spectrograph1)
       (calibration_target instrument0 groundstation2)
       (supports instrument1 thermograph0)
       (supports instrument1 spectrograph1)
       (supports instrument1 image2)
       (calibration_target instrument1 groundstation1)
       (supports instrument2 image2)
       (calibration_target instrument2 groundstation0)
       (on_board instrument0 satellite0)
       (on_board instrument1 satellite0)
       (on_board instrument2 satellite0)
       (power_avail satellite0)
       (pointing satellite0 phenomenon8))
  (:goal (and 
       (have_image planet9 spectrograph1)
       (have_image phenomenon8 image2)
       (have_image star7 thermograph0)
       (have_image phenomenon6 image2)
       (have_image phenomenon5 image2)
       (have_image star3 thermograph0)
       (pointing satellite0 phenomenon5))))