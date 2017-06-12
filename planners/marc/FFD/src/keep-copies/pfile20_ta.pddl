(define (problem depotprob7615) (:domain depot)
(:objects
	pallet5 - pallet
	pallet4 - pallet
	pallet7 - pallet
	pallet6 - pallet
	pallet1 - pallet
	pallet0 - pallet
	pallet3 - pallet
	pallet2 - pallet
	pallet9 - pallet
	pallet8 - pallet
	distributor1 - truck
	distributor1 - distributor
	crate11 - crate
	crate10 - crate
	crate13 - crate
	crate12 - crate
	crate14 - crate
	crate9 - crate
	crate8 - crate
	crate5 - crate
	crate4 - crate
	crate7 - crate
	crate6 - crate
	crate1 - crate
	crate0 - crate
	crate3 - crate
	crate2 - crate
	distributor1 - depot
	hoist0 - hoist
)
(:init
	(clear pallet9)
	(at crate5 distributor1)
	(at pallet4 distributor1)
	(at pallet5 distributor1)
	(clear crate14)
	(at crate7 distributor1)
	(at crate14 distributor1)
	(at pallet7 distributor1)
	(at crate13 distributor1)
	(on crate2 pallet3)
	(on crate10 crate0)
	(at crate12 distributor1)
	(on crate4 crate3)
	(at crate4 distributor1)
	(on crate13 crate4)
	(clear crate11)
	(at pallet1 distributor1)
	(at pallet0 distributor1)
	(at hoist0 distributor1)
	(at pallet6 distributor1)
	(clear crate13)
	(at crate9 distributor1)
	(at crate2 distributor1)
	(on crate7 pallet6)
	(on crate11 pallet8)
	(on crate3 pallet0)
	(at crate3 distributor1)
	(on crate14 crate12)
	(on crate9 crate1)
	(at distributor1 distributor1)
	(at crate0 distributor1)
	(clear crate5)
	(at crate11 distributor1)
	(at pallet2 distributor1)
	(on crate1 pallet5)
	(at crate6 distributor1)
	(at crate8 distributor1)
	(at crate10 distributor1)
	(on crate6 pallet1)
	(at crate1 distributor1)
	(on crate12 crate6)
	(on crate8 crate7)
	(at pallet9 distributor1)
	(available hoist0)
	(on crate0 pallet7)
	(clear pallet4)
	(on crate5 crate2)
	(clear pallet2)
	(clear crate9)
	(at pallet8 distributor1)
	(at pallet3 distributor1)
	(clear crate8)
	(clear crate10)
)
(:goal
	(and
		(on crate0 pallet3)
		(on crate1 crate11)
		(on crate2 pallet6)
		(on crate3 crate0)
		(on crate4 crate5)
		(on crate5 crate14)
		(on crate6 pallet4)
		(on crate7 pallet2)
		(on crate8 pallet7)
		(on crate9 crate8)
		(on crate11 pallet5)
		(on crate12 crate6)
		(on crate13 crate2)
		(on crate14 pallet1)
	)
)
)