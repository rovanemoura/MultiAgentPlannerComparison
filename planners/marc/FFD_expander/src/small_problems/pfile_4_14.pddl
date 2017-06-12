(define (problem tiny_problem) (:domain depot)
(:objects
	hoist1 - hoist
	crate13 - crate
	crate9 - crate
	driver1 - driver
	crate3 - crate
	crate4 - crate
	pallet2 - pallet
	crate1 - crate
	crate7 - crate
	truck1 - truck
	crate14 - crate
	crate5 - crate
	pallet0 - pallet
	distributor1 - distributor
	driver0 - driver
	crate6 - crate
	hoist2 - hoist
	crate11 - crate
	crate12 - crate
	hoist0 - hoist
	crate0 - crate
	depot0 - depot
	truck0 - truck
	pallet1 - pallet
	distributor0 - distributor
	crate8 - crate
	crate2 - crate
	crate10 - crate
)(:init
	(at hoist2 distributor1)
	(at pallet1 distributor0)
	(at hoist0 depot0)
	(driving driver0 truck0)
	(at pallet2 distributor1)
	(at hoist1 distributor0)
	(at pallet0 depot0)
	(driving driver1 truck1)
	(in crate14 truck0)
	(in crate13 truck0)
	(in crate7 truck0)
	(in crate6 truck0)
	(at crate8 depot0)
	(on crate8 pallet0)
	(at crate9 distributor0)
	(on crate9 pallet1)
	(at crate1 distributor0)
	(on crate1 crate9)
	(at crate2 distributor0)
	(on crate2 crate1)
	(at crate12 distributor1)
	(on crate12 pallet2)
	(at crate3 distributor1)
	(on crate3 crate12)
	(at crate11 distributor1)
	(on crate11 crate3)
	(clear crate2)
	(available distributor0 hoist1)
	(at truck1 depot0)
	(at crate0 depot0)
	(on crate0 crate8)
	(available depot0 hoist0)
	(at crate5 depot0)
	(clear crate5)
	(on crate5 crate0)
	(at truck0 distributor1)
	(in crate10 truck0)
	(lifting distributor1 hoist2 crate4)
	(clear crate11)
)(:goal (and 
	(clear crate4)
	(on crate0 crate8)
	(on crate1 crate9)
	(on crate12 pallet2)
	(on crate2 crate1)
	(on crate3 crate12)
	(on crate4 crate5)
	(on crate5 crate0)
	(on crate8 pallet0)
	(on crate9 pallet1)
)))