(define (problem depotprob190) (:domain depots)
(:objects
	depot0 depot1 depot2 depot3 - Depot
	distributor0 distributor1 distributor2 distributor3 - Distributor
	truck0 truck1 truck2 truck3 truck4 truck5 truck6 truck7 truck8 truck9 truck10 truck11 truck12 truck13 truck14 truck15 truck16 truck17 truck18 truck19 truck20 truck21 truck22 truck23 - Truck
	pallet0 pallet1 pallet2 pallet3 pallet4 pallet5 pallet6 pallet7 pallet8 pallet9 - Pallet
	crate0 crate1 crate2 crate3 crate4 crate5 crate6 crate7 crate8 crate9 crate10 crate11 crate12 crate13 crate14 - Crate
	hoist0 hoist1 hoist2 hoist3 hoist4 hoist5 hoist6 hoist7 - Hoist)
(:init
	(at pallet0 depot0)
	(clear crate6)
	(at pallet1 depot1)
	(clear crate1)
	(at pallet2 depot2)
	(clear crate12)
	(at pallet3 depot3)
	(clear crate14)
	(at pallet4 distributor0)
	(clear pallet4)
	(at pallet5 distributor1)
	(clear crate8)
	(at pallet6 distributor2)
	(clear crate13)
	(at pallet7 distributor3)
	(clear crate10)
	(at pallet8 distributor2)
	(clear crate5)
	(at pallet9 distributor2)
	(clear crate7)
	(at-truck truck0 distributor3)
	(at-truck truck1 distributor2)
	(at-truck truck2 depot1)
	(at-truck truck3 distributor1)
	(at-truck truck4 distributor1)
	(at-truck truck5 depot2)
	(at-truck truck6 depot0)
	(at-truck truck7 depot2)
	(at-truck truck8 distributor1)
	(at-truck truck9 depot1)
	(at-truck truck10 depot1)
	(at-truck truck11 depot0)
	(at-truck truck12 depot3)
	(at-truck truck13 distributor3)
	(at-truck truck14 distributor2)
	(at-truck truck15 depot2)
	(at-truck truck16 depot2)
	(at-truck truck17 distributor3)
	(at-truck truck18 distributor1)
	(at-truck truck19 distributor3)
	(at-truck truck20 depot3)
	(at-truck truck21 depot2)
	(at-truck truck22 depot1)
	(at-truck truck23 depot2)
	(at-hoist hoist0 depot0)
	(available hoist0)
	(at-hoist hoist1 depot1)
	(available hoist1)
	(at-hoist hoist2 depot2)
	(available hoist2)
	(at-hoist hoist3 depot3)
	(available hoist3)
	(at-hoist hoist4 distributor0)
	(available hoist4)
	(at-hoist hoist5 distributor1)
	(available hoist5)
	(at-hoist hoist6 distributor2)
	(available hoist6)
	(at-hoist hoist7 distributor3)
	(available hoist7)
	(at crate0 depot0)
	(on crate0 pallet0)
	(at crate1 depot1)
	(on crate1 pallet1)
	(at crate2 depot2)
	(on crate2 pallet2)
	(at crate3 distributor2)
	(on crate3 pallet9)
	(at crate4 distributor1)
	(on crate4 pallet5)
	(at crate5 distributor2)
	(on crate5 pallet8)
	(at crate6 depot0)
	(on crate6 crate0)
	(at crate7 distributor2)
	(on crate7 crate3)
	(at crate8 distributor1)
	(on crate8 crate4)
	(at crate9 depot3)
	(on crate9 pallet3)
	(at crate10 distributor3)
	(on crate10 pallet7)
	(at crate11 distributor2)
	(on crate11 pallet6)
	(at crate12 depot2)
	(on crate12 crate2)
	(at crate13 distributor2)
	(on crate13 crate11)
	(at crate14 depot3)
	(on crate14 crate9)
)

(:goal (and
		(on crate0 pallet9)
		(on crate1 crate5)
		(on crate2 crate10)
		(on crate4 crate8)
		(on crate5 crate6)
		(on crate6 pallet2)
		(on crate7 pallet4)
		(on crate8 pallet3)
		(on crate9 crate14)
		(on crate10 pallet7)
		(on crate11 crate12)
		(on crate12 pallet1)
		(on crate13 pallet5)
		(on crate14 pallet6)
	)
))