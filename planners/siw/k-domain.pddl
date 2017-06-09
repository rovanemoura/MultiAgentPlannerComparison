(define (domain Floods)
	(:requirements :typing)
(:types
	usv robot ugv uav - superduperagent
	robot area disaster store cdm box - object
	box superduperpred disaster area cdm object robot uav usv superduperagent superduperobject ugv store - superduperobject
	usv ugv uav - robot
)
(:constants
	pred--cdm_at pred--box_at_cdm pred--box_at_area pred--water_path pred--ground_path pred--visible_from pred--empty pred--full pred--have_water_sample_cdm pred--communicated_data pred--in_range pred--at pred--store_of pred--have_picture pred--box_at pred--have_water_sample  - superduperpred
)
(:predicates
	(cdm_at ?c - cdm ?a - area)
	(box_at_cdm ?b - box ?c - cdm)
	(box_at_area ?b - box ?a - area)
	(water_path ?a1 - area ?a2 - area)
	(ground_path ?a1 - area ?a2 - area)
	(visible_from ?d - disaster ?a - area)
	(empty ?s - store)
	(full ?s - store)
	(have_water_sample_cdm ?c - cdm ?a - area)
	(communicated_data ?d - disaster)
	(in_range ?a1 - area ?a2 - area)
	(at ?r - robot ?a - area)
	(store_of ?s - store ?r - robot)
	(have_picture ?r - robot ?d - disaster)
	(box_at ?b - box ?r - ugv)
	(have_water_sample ?r - usv ?a - area)
	(K-obj ?ag - superduperagent ?obj - superduperobject)
	(K-pred ?ag - superduperagent ?pr - superduperpred)
	(K-ag-pred ?ag - superduperagent ?pr - superduperpred)
)

(:action navigate_usv
	:parameters (?r - usv ?a1 - area ?a2 - area)
	:precondition (and
		(at ?r ?a1)
		(water_path ?a1 ?a2)
		(K-ag-pred ?r pred--at)
		(K-pred ?r pred--water_path)
		(K-obj ?r ?a1)
		(K-obj ?r ?a2)
	)
	:effect (and
		(not (at ?r ?a1))
		(at ?r ?a2)
	)
)


(:action navigate_ugv
	:parameters (?r - ugv ?a1 - area ?a2 - area)
	:precondition (and
		(at ?r ?a1)
		(ground_path ?a1 ?a2)
		(K-pred ?r pred--ground_path)
		(K-ag-pred ?r pred--at)
		(K-obj ?r ?a1)
		(K-obj ?r ?a2)
	)
	:effect (and
		(not (at ?r ?a1))
		(at ?r ?a2)
	)
)


(:action navigate_uav
	:parameters (?r - uav ?a1 - area ?a2 - area)
	:precondition (and
		(at ?r ?a1)
		(K-ag-pred ?r pred--at)
		(K-obj ?r ?a1)
		(K-obj ?r ?a2)
	)
	:effect (and
		(not (at ?r ?a1))
		(at ?r ?a2)
	)
)


(:action take_picture
	:parameters (?r - robot ?a - area ?d - disaster)
	:precondition (and
		(visible_from ?d ?a)
		(at ?r ?a)
		(K-ag-pred ?r pred--have_picture)
		(K-pred ?r pred--visible_from)
		(K-ag-pred ?r pred--at)
		(K-obj ?r ?a)
		(K-obj ?r ?d)
	)
	:effect 
		(have_picture ?r ?d)
)


(:action sample_water
	:parameters (?r - usv ?s - store ?a - area)
	:precondition (and
		(at ?r ?a)
		(store_of ?s ?r)
		(empty ?s)
		(K-ag-pred ?r pred--have_water_sample)
		(K-pred ?r pred--full)
		(K-pred ?r pred--empty)
		(K-ag-pred ?r pred--at)
		(K-ag-pred ?r pred--store_of)
		(K-obj ?r ?s)
		(K-obj ?r ?a)
	)
	:effect (and
		(not (empty ?s))
		(full ?s)
		(have_water_sample ?r ?a)
	)
)


(:action drop_sample
	:parameters (?r - usv ?s - store ?a1 - area ?a2 - area ?c - cdm)
	:precondition (and
		(store_of ?s ?r)
		(have_water_sample ?r ?a2)
		(cdm_at ?c ?a1)
		(at ?r ?a1)
		(K-ag-pred ?r pred--have_water_sample)
		(K-ag-pred ?r pred--at)
		(K-pred ?r pred--full)
		(K-pred ?r pred--have_water_sample_cdm)
		(K-ag-pred ?r pred--store_of)
		(K-pred ?r pred--cdm_at)
		(K-pred ?r pred--empty)
		(K-obj ?r ?s)
		(K-obj ?r ?a1)
		(K-obj ?r ?a2)
		(K-obj ?r ?c)
	)
	:effect (and
		(not (full ?s))
		(not (have_water_sample ?r ?a2))
		(empty ?s)
		(have_water_sample_cdm ?c ?a2)
	)
)


(:action pickup_box
	:parameters (?r - ugv ?s - store ?c - cdm ?a - area ?b - box)
	:precondition (and
		(box_at_cdm ?b ?c)
		(cdm_at ?c ?a)
		(at ?r ?a)
		(store_of ?s ?r)
		(empty ?s)
		(K-pred ?r pred--box_at_cdm)
		(K-ag-pred ?r pred--box_at)
		(K-pred ?r pred--full)
		(K-ag-pred ?r pred--store_of)
		(K-pred ?r pred--cdm_at)
		(K-pred ?r pred--empty)
		(K-ag-pred ?r pred--at)
		(K-obj ?r ?s)
		(K-obj ?r ?c)
		(K-obj ?r ?a)
		(K-obj ?r ?b)
	)
	:effect (and
		(not (empty ?s))
		(not (box_at_cdm ?b ?c))
		(full ?s)
		(box_at ?b ?r)
	)
)


(:action drop_box
	:parameters (?r - ugv ?s - store ?a - area ?b - box)
	:precondition (and
		(box_at ?b ?r)
		(store_of ?s ?r)
		(at ?r ?a)
		(K-ag-pred ?r pred--at)
		(K-pred ?r pred--box_at_area)
		(K-ag-pred ?r pred--box_at)
		(K-pred ?r pred--full)
		(K-ag-pred ?r pred--store_of)
		(K-pred ?r pred--empty)
		(K-obj ?r ?s)
		(K-obj ?r ?a)
		(K-obj ?r ?b)
	)
	:effect (and
		(not (full ?s))
		(not (box_at ?b ?r))
		(empty ?s)
		(box_at_area ?b ?a)
	)
)


(:action communicate_data
	:parameters (?r - robot ?c - cdm ?d - disaster ?a1 - area ?a2 - area)
	:precondition (and
		(at ?r ?a1)
		(cdm_at ?c ?a2)
		(have_picture ?r ?d)
		(in_range ?a1 ?a2)
		(K-ag-pred ?r pred--have_picture)
		(K-pred ?r pred--communicated_data)
		(K-ag-pred ?r pred--at)
		(K-pred ?r pred--cdm_at)
		(K-pred ?r pred--in_range)
		(K-obj ?r ?c)
		(K-obj ?r ?d)
		(K-obj ?r ?a1)
		(K-obj ?r ?a2)
	)
	:effect (and
		(not (have_picture ?r ?d))
		(communicated_data ?d)
	)
)

)