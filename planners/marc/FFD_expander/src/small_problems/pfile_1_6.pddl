(define (problem tiny_problem) (:domain satellite)
(:objects
	star12 - direction
	instrument12 - instrument
	planet31 - direction
	planet63 - direction
	instrument0 - instrument
	instrument10 - instrument
	planet61 - direction
	star25 - direction
	star4 - direction
	instrument5 - instrument
	instrument15 - instrument
	satellite3 - satellite
	planet32 - direction
	star19 - direction
	image2 - mode
	planet18 - direction
	star54 - direction
	star11 - direction
	phenomenon8 - direction
	star41 - direction
	satellite2 - satellite
	planet67 - direction
	groundstation0 - direction
	planet60 - direction
	satellite5 - satellite
	star23 - direction
	phenomenon15 - direction
	star20 - direction
	satellite6 - satellite
	planet62 - direction
	phenomenon26 - direction
	phenomenon22 - direction
	phenomenon58 - direction
	phenomenon71 - direction
	phenomenon6 - direction
	planet27 - direction
	instrument13 - instrument
	phenomenon29 - direction
	planet66 - direction
	planet36 - direction
	thermograph0 - mode
	planet51 - direction
	star21 - direction
	instrument1 - instrument
	instrument9 - instrument
	planet57 - direction
	star64 - direction
	star46 - direction
	star48 - direction
	groundstation2 - direction
	star65 - direction
	planet68 - direction
	star47 - direction
	satellite4 - satellite
	phenomenon28 - direction
	star37 - direction
	instrument16 - instrument
	spectrograph1 - mode
	planet53 - direction
	satellite7 - satellite
	phenomenon42 - direction
	planet39 - direction
	satellite1 - satellite
	phenomenon33 - direction
	star38 - direction
	star56 - direction
	phenomenon13 - direction
	star3 - direction
	instrument11 - instrument
	instrument7 - instrument
	groundstation1 - direction
	planet35 - direction
	star7 - direction
	star50 - direction
	star45 - direction
	instrument2 - instrument
	star52 - direction
	star59 - direction
	planet9 - direction
	phenomenon43 - direction
	planet30 - direction
	instrument6 - instrument
	star14 - direction
	instrument14 - instrument
	phenomenon5 - direction
	planet72 - direction
	phenomenon10 - direction
	planet16 - direction
	star69 - direction
	instrument3 - instrument
	star17 - direction
	star34 - direction
	star44 - direction
	phenomenon49 - direction
	instrument8 - instrument
	phenomenon70 - direction
	phenomenon40 - direction
	star24 - direction
	instrument4 - instrument
	satellite0 - satellite
	phenomenon55 - direction
)(:init
	(supports instrument5 image2)
	(pointing satellite7 star34)
	(pointing satellite2 phenomenon58)
	(supports instrument8 spectrograph1)
	(supports instrument15 thermograph0)
	(supports instrument5 thermograph0)
	(on_board instrument8 satellite4)
	(calibration_target instrument9 groundstation2)
	(pointing satellite3 phenomenon15)
	(on_board instrument15 satellite7)
	(on_board instrument4 satellite2)
	(on_board instrument0 satellite0)
	(on_board instrument16 satellite7)
	(calibration_target instrument0 groundstation2)
	(pointing satellite5 planet27)
	(supports instrument1 thermograph0)
	(calibration_target instrument3 groundstation2)
	(calibration_target instrument11 groundstation1)
	(supports instrument11 spectrograph1)
	(supports instrument0 thermograph0)
	(supports instrument7 image2)
	(supports instrument6 thermograph0)
	(supports instrument4 image2)
	(calibration_target instrument10 groundstation2)
	(on_board instrument7 satellite3)
	(calibration_target instrument12 groundstation1)
	(supports instrument0 image2)
	(supports instrument16 thermograph0)
	(supports instrument3 image2)
	(supports instrument2 image2)
	(pointing satellite1 star7)
	(calibration_target instrument16 groundstation2)
	(supports instrument16 image2)
	(pointing satellite4 planet31)
	(supports instrument7 thermograph0)
	(calibration_target instrument4 groundstation1)
	(calibration_target instrument13 groundstation1)
	(calibration_target instrument14 groundstation2)
	(calibration_target instrument7 groundstation0)
	(calibration_target instrument2 groundstation1)
	(on_board instrument10 satellite4)
	(supports instrument1 spectrograph1)
	(power_avail satellite2)
	(supports instrument13 spectrograph1)
	(supports instrument9 image2)
	(supports instrument6 image2)
	(supports instrument10 spectrograph1)
	(on_board instrument12 satellite6)
	(power_avail satellite1)
	(on_board instrument14 satellite6)
	(supports instrument8 image2)
	(supports instrument5 spectrograph1)
	(on_board instrument13 satellite6)
	(calibration_target instrument8 groundstation0)
	(on_board instrument5 satellite2)
	(power_avail satellite3)
	(on_board instrument2 satellite1)
	(supports instrument11 image2)
	(on_board instrument9 satellite4)
	(on_board instrument1 satellite1)
	(supports instrument3 spectrograph1)
	(calibration_target instrument1 groundstation0)
	(supports instrument7 spectrograph1)
	(supports instrument10 thermograph0)
	(supports instrument4 thermograph0)
	(supports instrument4 spectrograph1)
	(supports instrument0 spectrograph1)
	(supports instrument14 thermograph0)
	(on_board instrument11 satellite5)
	(supports instrument12 image2)
	(supports instrument14 spectrograph1)
	(supports instrument14 image2)
	(on_board instrument3 satellite1)
	(supports instrument9 thermograph0)
	(supports instrument16 spectrograph1)
	(supports instrument8 thermograph0)
	(power_avail satellite7)
	(supports instrument10 image2)
	(on_board instrument6 satellite3)
	(calibration_target instrument5 groundstation1)
	(power_avail satellite4)
	(supports instrument2 spectrograph1)
	(calibration_target instrument15 groundstation2)
	(calibration_target instrument6 groundstation2)
	(supports instrument1 image2)
	(power_avail satellite5)
	(supports instrument15 image2)
	(supports instrument3 thermograph0)
	(supports instrument11 thermograph0)
	(power_on instrument0)
	(power_on instrument13)
	(calibrated instrument13)
	(have_image phenomenon22 spectrograph1)
	(have_image planet27 spectrograph1)
	(calibrated instrument0)
	(have_image planet31 thermograph0)
	(have_image star34 thermograph0)
	(have_image star44 thermograph0)
	(have_image star7 thermograph0)
	(have_image phenomenon10 thermograph0)
	(have_image phenomenon13 thermograph0)
	(have_image phenomenon15 spectrograph1)
	(have_image phenomenon26 spectrograph1)
	(have_image phenomenon33 spectrograph1)
	(have_image phenomenon42 spectrograph1)
	(have_image phenomenon43 thermograph0)
	(have_image planet32 thermograph0)
	(have_image planet35 image2)
	(have_image phenomenon49 image2)
	(have_image phenomenon5 image2)
	(have_image phenomenon55 thermograph0)
	(have_image phenomenon6 image2)
	(have_image phenomenon70 spectrograph1)
	(have_image phenomenon71 spectrograph1)
	(have_image phenomenon8 image2)
	(have_image planet16 image2)
	(have_image planet18 image2)
	(have_image planet36 thermograph0)
	(have_image planet39 image2)
	(pointing satellite6 planet51)
	(have_image planet51 spectrograph1)
	(have_image planet53 thermograph0)
	(pointing satellite0 planet57)
	(have_image planet57 thermograph0)
)(:goal (and 
	(have_image phenomenon10 thermograph0)
	(have_image phenomenon13 thermograph0)
	(have_image phenomenon15 spectrograph1)
	(have_image phenomenon22 spectrograph1)
	(have_image phenomenon26 spectrograph1)
	(have_image phenomenon33 spectrograph1)
	(have_image phenomenon42 spectrograph1)
	(have_image phenomenon43 thermograph0)
	(have_image phenomenon49 image2)
	(have_image phenomenon5 image2)
	(have_image phenomenon55 thermograph0)
	(have_image phenomenon6 image2)
	(have_image phenomenon70 spectrograph1)
	(have_image phenomenon71 spectrograph1)
	(have_image phenomenon8 image2)
	(have_image planet16 image2)
	(have_image planet18 image2)
	(have_image planet27 spectrograph1)
	(have_image planet31 thermograph0)
	(have_image planet32 thermograph0)
	(have_image planet35 image2)
	(have_image planet36 thermograph0)
	(have_image planet39 image2)
	(have_image planet51 spectrograph1)
	(have_image planet53 thermograph0)
	(have_image planet57 thermograph0)
	(have_image planet60 thermograph0)
	(have_image star34 thermograph0)
	(have_image star44 thermograph0)
	(have_image star7 thermograph0)
)))