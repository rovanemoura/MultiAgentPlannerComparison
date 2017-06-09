(define (problem truck1-pfile03-ma) (:domain depots)
  (:objects hoist0 hoist1 hoist2 - hoist
             crate0 crate1 crate2 crate3 crate4 crate5 - crate
             pallet0 pallet1 pallet2 - pallet
             truck1 - truck
             distributor0 distributor1 - distributor
             depot0 - depot
            )
  (:init
       (at pallet0 depot0)
       (clear crate1)
       (at pallet1 distributor0)
       (clear crate4)
       (at pallet2 distributor1)
       (clear crate5)
       (at-truck truck1 distributor0)
       (at-hoist hoist0 depot0)
       (available hoist0)
       (at-hoist hoist1 distributor0)
       (available hoist1)
       (at-hoist hoist2 distributor1)
       (available hoist2)
       (at crate0 distributor0)
       (on crate0 pallet1)
       (at crate1 depot0)
       (on crate1 pallet0)
       (at crate2 distributor1)
       (on crate2 pallet2)
       (at crate3 distributor0)
       (on crate3 crate0)
       (at crate4 distributor0)
       (on crate4 crate3)
       (at crate5 distributor1)
       (on crate5 crate2))
  (:goal (and 
       (on crate5 crate0)
       (on crate3 crate2))))