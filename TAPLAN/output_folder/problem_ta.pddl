(define (problem floodsp10) (:domain Floods)
(:objects
	box1 - box
	box2 - box
	box3 - box
	box4 - box
	box5 - box
	box6 - box
	box7 - box
	box8 - box
	box9 - box
	box10 - box
	uav1 - uav
	area1 - area
	area2 - area
	area3 - area
	area4 - area
	area5 - area
	area6 - area
	area7 - area
	area8 - area
	area9 - area
	area10 - area
	area11 - area
	area12 - area
	area13 - area
	area14 - area
	area15 - area
	area16 - area
	area17 - area
	area18 - area
	area19 - area
	area20 - area
	area21 - area
	area22 - area
	area23 - area
	area24 - area
	area25 - area
	area26 - area
	area27 - area
	area28 - area
	area29 - area
	area30 - area
	area31 - area
	area32 - area
	area33 - area
	area34 - area
	area35 - area
	area36 - area
	area37 - area
	area38 - area
	area39 - area
	area40 - area
	area41 - area
	area42 - area
	area43 - area
	area44 - area
	area45 - area
	area46 - area
	area47 - area
	area48 - area
	area49 - area
	area50 - area
	area51 - area
	area52 - area
	area53 - area
	area54 - area
	area55 - area
	area56 - area
	area57 - area
	area58 - area
	area59 - area
	area60 - area
	ugv1Store - store
	ugv2Store - store
	ugv3Store - store
	ugv4Store - store
	ugv5Store - store
	ugv6Store - store
	ugv7Store - store
	ugv8Store - store
	ugv9Store - store
	ugv10Store - store
	ugv11Store - store
	ugv12Store - store
	usv1Store - store
	usv2Store - store
	usv3Store - store
	usv4Store - store
	usv5Store - store
	usv6Store - store
	usv7Store - store
	usv8Store - store
	usv9Store - store
	usv10Store - store
	usv11Store - store
	usv12Store - store
	uav1 - usv
	cdm1 - cdm
	cdm2 - cdm
	cdm3 - cdm
	cdm4 - cdm
	cdm5 - cdm
	cdm6 - cdm
	uav1 - ugv
	disaster1 - disaster
	disaster2 - disaster
	disaster3 - disaster
	disaster4 - disaster
	disaster5 - disaster
	disaster6 - disaster
	disaster7 - disaster
	disaster8 - disaster
	disaster9 - disaster
	disaster10 - disaster
	disaster11 - disaster
	disaster12 - disaster
	disaster13 - disaster
	disaster14 - disaster
	disaster15 - disaster
	disaster16 - disaster
)
(:init
	(store_of usv5Store uav1)
	(water_path area14 area16)
	(water_path area20 area19)
	(cdm_at cdm3 area21)
	(water_path area16 area14)
	(water_path area42 area54)
	(ground_path area25 area23)
	(ground_path area16 area17)
	(ground_path area14 area17)
	(ground_path area28 area27)
	(ground_path area43 area45)
	(in_range area21 area21)
	(store_of ugv7Store uav1)
	(water_path area40 area39)
	(ground_path area55 area58)
	(water_path area38 area40)
	(ground_path area3 area1)
	(ground_path area26 area24)
	(empty ugv6Store)
	(ground_path area43 area41)
	(empty ugv9Store)
	(water_path area4 area5)
	(ground_path area30 area24)
	(in_range area43 area41)
	(ground_path area44 area43)
	(ground_path area36 area35)
	(ground_path area44 area50)
	(water_path area18 area16)
	(ground_path area53 area13)
	(in_range area3 area1)
	(water_path area38 area17)
	(water_path area9 area13)
	(ground_path area10 area4)
	(visible_from disaster11 area24)
	(in_range area53 area53)
	(water_path area38 area36)
	(box_at_cdm box1 cdm1)
	(ground_path area47 area48)
	(in_range area33 area33)
	(in_range area22 area21)
	(ground_path area13 area10)
	(water_path area17 area38)
	(water_path area2 area6)
	(ground_path area49 area51)
	(ground_path area60 area59)
	(water_path area2 area1)
	(in_range area49 area53)
	(at uav1 area53)
	(water_path area39 area40)
	(in_range area50 area53)
	(empty usv4Store)
	(box_at_cdm box5 cdm3)
	(box_at_cdm box10 cdm6)
	(water_path area12 area10)
	(ground_path area20 area19)
	(ground_path area3 area4)
	(visible_from disaster7 area55)
	(ground_path area58 area55)
	(water_path area49 area44)
	(water_path area1 area2)
	(water_path area28 area27)
	(water_path area13 area53)
	(ground_path area4 area6)
	(water_path area44 area45)
	(water_path area20 area18)
	(ground_path area35 area38)
	(ground_path area31 area29)
	(water_path area21 area22)
	(water_path area47 area48)
	(store_of usv9Store uav1)
	(box_at_cdm box6 cdm4)
	(water_path area16 area18)
	(water_path area59 area60)
	(water_path area21 area36)
	(ground_path area33 area30)
	(ground_path area51 area45)
	(ground_path area10 area7)
	(water_path area32 area26)
	(ground_path area60 area58)
	(water_path area1 area16)
	(visible_from disaster5 area13)
	(empty usv9Store)
	(water_path area2 area14)
	(visible_from disaster6 area7)
	(water_path area49 area48)
	(ground_path area29 area31)
	(water_path area40 area38)
	(ground_path area59 area60)
	(ground_path area16 area15)
	(store_of usv6Store uav1)
	(water_path area8 area7)
	(water_path area30 area32)
	(ground_path area27 area28)
	(water_path area58 area56)
	(water_path area38 area35)
	(ground_path area38 area40)
	(store_of ugv5Store uav1)
	(ground_path area5 area11)
	(ground_path area1 area3)
	(ground_path area33 area53)
	(in_range area9 area13)
	(ground_path area51 area49)
	(water_path area22 area34)
	(box_at_cdm box8 cdm5)
	(store_of ugv9Store uav1)
	(water_path area41 area56)
	(water_path area13 area9)
	(water_path area54 area56)
	(water_path area53 area49)
	(store_of ugv1Store uav1)
	(at uav1 area33)
	(water_path area42 area41)
	(ground_path area17 area16)
	(ground_path area25 area31)
	(ground_path area44 area46)
	(water_path area52 area46)
	(ground_path area45 area51)
	(empty usv11Store)
	(ground_path area57 area56)
	(ground_path area57 area54)
	(water_path area19 area20)
	(visible_from disaster4 area36)
	(visible_from disaster10 area37)
	(ground_path area50 area47)
	(water_path area60 area59)
	(water_path area55 area58)
	(ground_path area18 area37)
	(visible_from disaster12 area30)
	(visible_from disaster14 area38)
	(water_path area45 area44)
	(water_path area48 area47)
	(water_path area29 area33)
	(ground_path area40 area38)
	(in_range area30 area33)
	(water_path area36 area34)
	(water_path area44 area42)
	(in_range area2 area1)
	(water_path area4 area9)
	(water_path area57 area59)
	(water_path area34 area22)
	(water_path area6 area2)
	(water_path area37 area39)
	(ground_path area7 area10)
	(ground_path area15 area18)
	(ground_path area11 area5)
	(ground_path area23 area25)
	(water_path area33 area53)
	(ground_path area30 area33)
	(ground_path area45 area43)
	(water_path area2 area4)
	(water_path area24 area29)
	(ground_path area3 area15)
	(water_path area36 area21)
	(box_at_cdm box3 cdm1)
	(store_of usv7Store uav1)
	(empty usv2Store)
	(in_range area41 area41)
	(ground_path area31 area25)
	(ground_path area23 area35)
	(ground_path area41 area56)
	(visible_from disaster2 area27)
	(empty ugv8Store)
	(cdm_at cdm5 area41)
	(ground_path area7 area8)
	(ground_path area23 area21)
	(ground_path area41 area43)
	(store_of ugv3Store uav1)
	(store_of ugv11Store uav1)
	(water_path area53 area13)
	(water_path area48 area49)
	(ground_path area53 area50)
	(water_path area53 area33)
	(water_path area25 area24)
	(water_path area52 area50)
	(water_path area58 area60)
	(cdm_at cdm1 area1)
	(water_path area32 area30)
	(water_path area4 area2)
	(ground_path area17 area14)
	(store_of ugv12Store uav1)
	(empty usv5Store)
	(at uav1 area41)
	(ground_path area9 area11)
	(ground_path area18 area20)
	(water_path area36 area38)
	(ground_path area40 area39)
	(water_path area24 area22)
	(water_path area29 area28)
	(water_path area22 area26)
	(water_path area54 area42)
	(water_path area17 area19)
	(empty usv6Store)
	(at uav1 area1)
	(store_of usv2Store uav1)
	(water_path area14 area2)
	(water_path area19 area17)
	(ground_path area10 area13)
	(empty usv3Store)
	(ground_path area1 area16)
	(ground_path area30 area27)
	(empty ugv2Store)
	(water_path area10 area12)
	(water_path area46 area42)
	(ground_path area43 area44)
	(visible_from disaster15 area20)
	(ground_path area3 area5)
	(visible_from disaster8 area11)
	(ground_path area24 area23)
	(in_range area29 area33)
	(ground_path area18 area15)
	(water_path area9 area4)
	(ground_path area21 area36)
	(ground_path area47 area50)
	(ground_path area13 area53)
	(water_path area41 area42)
	(store_of ugv10Store uav1)
	(ground_path area37 area34)
	(water_path area18 area20)
	(in_range area1 area1)
	(cdm_at cdm4 area33)
	(ground_path area39 area40)
	(water_path area46 area52)
	(visible_from disaster13 area44)
	(water_path area27 area8)
	(empty ugv11Store)
	(visible_from disaster1 area58)
	(visible_from disaster16 area28)
	(ground_path area15 area16)
	(water_path area34 area36)
	(ground_path area50 area44)
	(empty ugv4Store)
	(water_path area56 area58)
	(water_path area49 area53)
	(store_of ugv4Store uav1)
	(empty ugv5Store)
	(store_of usv3Store uav1)
	(ground_path area11 area9)
	(store_of ugv6Store uav1)
	(box_at_cdm box4 cdm4)
	(water_path area27 area28)
	(ground_path area55 area43)
	(ground_path area53 area33)
	(water_path area16 area1)
	(store_of usv4Store uav1)
	(water_path area56 area41)
	(store_of usv1Store uav1)
	(water_path area24 area25)
	(water_path area42 area46)
	(ground_path area46 area44)
	(box_at_cdm box2 cdm2)
	(ground_path area16 area1)
	(water_path area8 area9)
	(ground_path area50 area53)
	(empty usv7Store)
	(ground_path area4 area10)
	(water_path area22 area24)
	(ground_path area58 area60)
	(in_range area10 area13)
	(empty ugv1Store)
	(empty usv1Store)
	(at uav1 area13)
	(water_path area8 area27)
	(water_path area50 area52)
	(ground_path area37 area36)
	(ground_path area54 area57)
	(store_of usv10Store uav1)
	(water_path area56 area54)
	(empty usv8Store)
	(box_at_cdm box9 cdm6)
	(water_path area39 area37)
	(water_path area12 area6)
	(ground_path area56 area41)
	(ground_path area55 area56)
	(box_at_cdm box7 cdm5)
	(water_path area60 area58)
	(ground_path area56 area57)
	(store_of usv12Store uav1)
	(water_path area58 area55)
	(water_path area5 area4)
	(store_of usv8Store uav1)
	(empty usv10Store)
	(water_path area15 area18)
	(ground_path area19 area20)
	(ground_path area7 area28)
	(empty ugv12Store)
	(water_path area18 area15)
	(water_path area42 area44)
	(water_path area6 area12)
	(water_path area33 area29)
	(ground_path area36 area21)
	(ground_path area34 area37)
	(visible_from disaster9 area41)
	(store_of ugv8Store uav1)
	(ground_path area6 area4)
	(ground_path area24 area30)
	(ground_path area35 area23)
	(ground_path area38 area35)
	(ground_path area48 area47)
	(ground_path area56 area55)
	(ground_path area5 area3)
	(water_path area26 area22)
	(at uav1 area21)
	(ground_path area21 area23)
	(ground_path area27 area30)
	(ground_path area4 area3)
	(ground_path area8 area7)
	(water_path area9 area8)
	(ground_path area23 area24)
	(ground_path area24 area26)
	(ground_path area28 area7)
	(ground_path area37 area18)
	(store_of ugv2Store uav1)
	(water_path area29 area24)
	(ground_path area36 area37)
	(water_path area7 area8)
	(water_path area28 area29)
	(cdm_at cdm6 area53)
	(water_path area22 area21)
	(empty usv12Store)
	(visible_from disaster3 area49)
	(in_range area13 area13)
	(water_path area26 area32)
	(store_of usv11Store uav1)
	(cdm_at cdm2 area13)
	(water_path area44 area49)
	(ground_path area20 area18)
	(ground_path area35 area36)
	(ground_path area43 area55)
	(empty ugv10Store)
	(in_range area23 area21)
	(water_path area35 area38)
	(in_range area42 area41)
	(empty ugv7Store)
	(water_path area59 area57)
	(empty ugv3Store)
	(ground_path area15 area3)
)
(:goal
	(and
		(communicated_data disaster1)
		(communicated_data disaster2)
		(communicated_data disaster3)
		(communicated_data disaster4)
		(communicated_data disaster5)
		(communicated_data disaster6)
		(communicated_data disaster7)
		(communicated_data disaster8)
		(communicated_data disaster9)
		(communicated_data disaster10)
		(communicated_data disaster11)
		(communicated_data disaster12)
		(communicated_data disaster13)
		(communicated_data disaster14)
		(communicated_data disaster15)
		(communicated_data disaster16)
		(box_at_area box1 area29)
		(box_at_area box2 area33)
		(box_at_area box3 area39)
		(box_at_area box4 area54)
		(box_at_area box5 area6)
		(box_at_area box6 area31)
		(box_at_area box7 area7)
		(box_at_area box8 area21)
		(box_at_area box9 area15)
		(box_at_area box10 area18)
		(have_water_sample_cdm cdm2 area35)
		(have_water_sample_cdm cdm1 area40)
		(have_water_sample_cdm cdm3 area2)
		(have_water_sample_cdm cdm3 area17)
		(have_water_sample_cdm cdm4 area8)
		(have_water_sample_cdm cdm5 area28)
		(have_water_sample_cdm cdm6 area4)
		(have_water_sample_cdm cdm6 area56)
		(have_water_sample_cdm cdm5 area33)
		(have_water_sample_cdm cdm1 area37)
	)
)
)