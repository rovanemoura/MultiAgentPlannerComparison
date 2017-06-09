(define (problem tiny_problem) (:domain depot)
(:objects
	distributor1 - distributor
	distributor0 - distributor
	distributor2 - distributor
	pallet3 - pallet
	pallet2 - pallet
	pallet1 - pallet
	pallet0 - pallet
	truck1 - truck
	truck0 - truck
	pallet5 - pallet
	pallet4 - pallet
	crate1 - crate
	crate0 - crate
	hoist5 - hoist
	hoist4 - hoist
	hoist1 - hoist
	hoist0 - hoist
	hoist3 - hoist
	hoist2 - hoist
	driver1 - driver
	crate2 - crate
	crate5 - crate
	crate4 - crate
	crate3 - crate
	depot0 - depot
	depot1 - depot
	depot2 - depot
	driver0 - driver
)(:init
	(at hoist4 distributor1)
	(at pallet5 distributor2)
	(at hoist3 distributor0)
	(available distributor1 hoist4)
	(at truck1 depot2)
	(at hoist5 distributor2)
	(at pallet4 distributor1)
	(at pallet1 depot1)
	(at hoist2 depot2)
	(at crate4 depot2)
	(on crate4 pallet2)
	(driving driver1 truck1)
	(at hoist1 depot1)
	(at hoist0 depot0)
	(at pallet2 depot2)
	(clear pallet4)
	(at pallet3 distributor0)
	(at pallet0 depot0)
	(driving driver0 truck0)
	(clear pallet1)
	(available depot1 hoist1)
	(in crate1 truck0)
	(available depot0 hoist0)
	(at crate3 depot0)
	(clear crate3)
	(on crate3 pallet0)
	(clear pallet5)
	(available distributor2 hoist5)
	(at truck0 distributor0)
	(in crate5 truck0)
	(available distributor0 hoist3)
	(at crate2 distributor0)
	(clear crate2)
	(on crate2 pallet3)
	(lifting depot2 hoist2 crate0)
	(clear crate4)
)(:goal (and 
	(in crate0 truck0)
	(on crate0 crate4)
	(on crate3 pallet0)
	(on crate2 pallet3)
)))