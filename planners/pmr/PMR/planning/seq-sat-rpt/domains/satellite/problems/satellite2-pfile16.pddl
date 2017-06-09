(define (problem pfile16) (:domain satellite)
  (:objects star24 phenomenon23 planet22 planet21 star20 planet19 phenomenon18 phenomenon17 star16 star15 star14 star13 star12 planet11 planet10 phenomenon9 planet8 planet7 planet6 phenomenon5 groundstation1 star2 groundstation4 star3 groundstation0 - direction
             spectrograph3 thermograph1 infrared4 image2 image0 - mode
             instrument6 - instrument
             satellite2 - satellite
            )
  (:init
       (supports instrument6 image0)
       (calibration_target instrument6 groundstation1)
       (on_board instrument6 satellite2)
       (power_avail satellite2)
       (pointing satellite2 star24))
  (:goal (and 
       (have_image phenomenon23 image0)
       (have_image star20 image0)
       (have_image star15 image0)
       (have_image star13 image0)
       (have_image star12 image0)
       (have_image planet10 image0)
       (have_image planet7 image0))))