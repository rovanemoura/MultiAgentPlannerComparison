(define (domain Floods)
	(:requirements :typing)
(:types
	robot area disaster store cdm box - object
	usv ugv uav - robot
)
(:predicates
	(box_at_area ?b - box ?a - area)
	(cdm_at ?c - cdm ?a - area)
	(water_path ?a1 - area ?a2 - area)
	(visible_from ?d - disaster ?a - area)
	(in_range ?a1 - area ?a2 - area)
	(box_at ?b - box ?r - ugv)
	(have_water_sample ?r - usv ?a - area)
	(empty ?s - store)
	(have_water_sample_cdm ?c - cdm ?a - area)
	(ground_path ?a1 - area ?a2 - area)
	(full ?s - store)
	(communicated_data ?d - disaster)
	(have_picture ?r - robot ?d - disaster)
	(at ?r - robot ?a - area)
	(box_at_cdm ?b - box ?c - cdm)
	(store_of ?s - store ?r - robot)
)

(:action navigate_usv
	:parameters (?r - usv ?a1 - area ?a2 - area)
	:precondition (and
		(at ?r ?a1)
		(water_path ?a1 ?a2)
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
	)
	:effect (and
		(not (at ?r ?a1))
		(at ?r ?a2)
	)
)


(:action navigate_uav
	:parameters (?r - uav ?a1 - area ?a2 - area)
	:precondition 
		(at ?r ?a1)
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
	)
	:effect (and
		(not (have_picture ?r ?d))
		(communicated_data ?d)
	)
)

)