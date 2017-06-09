(define (problem floodsp01) (:domain floods)
  (:objects cdm1 - cdm
             cdm2 - cdm
             ugv1store - store
             ugv2store - store
             ugv3store - store
             usv1store - store
             usv2store - store
             usv3store - store
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
             disaster1 - disaster
             disaster2 - disaster
             disaster3 - disaster
             disaster4 - disaster
             disaster5 - disaster
             box1 - box
             box2 - box
             uav1 - uav
             uav2 - uav
             uav3 - uav
             ugv1 - ugv
             ugv2 - ugv
             ugv3 - ugv
             usv1 - usv
             usv2 - usv
             usv3 - usv
            )
  (:init
       (store_of ugv1store ugv1)
       (store_of ugv2store ugv2)
       (store_of ugv3store ugv3)
       (store_of usv1store usv1)
       (store_of usv2store usv2)
       (store_of usv3store usv3)
       (empty ugv1store)
       (empty ugv2store)
       (empty ugv3store)
       (empty usv1store)
       (empty usv2store)
       (empty usv3store)
       (cdm_at cdm1 area1)
       (cdm_at cdm2 area13)
       (at uav1 area1)
       (at uav2 area1)
       (at uav3 area13)
       (at usv1 area1)
       (at usv2 area1)
       (at usv3 area13)
       (at ugv1 area1)
       (at ugv2 area1)
       (at ugv3 area13)
       (box_at_cdm box1 cdm1)
       (box_at_cdm box2 cdm2)
       (water_path area1 area2)
       (water_path area2 area1)
       (water_path area2 area4)
       (water_path area4 area2)
       (water_path area2 area6)
       (water_path area6 area2)
       (water_path area4 area5)
       (water_path area5 area4)
       (water_path area4 area9)
       (water_path area9 area4)
       (water_path area9 area8)
       (water_path area8 area9)
       (water_path area8 area7)
       (water_path area7 area8)
       (water_path area6 area12)
       (water_path area12 area6)
       (water_path area12 area10)
       (water_path area10 area12)
       (water_path area9 area13)
       (water_path area13 area9)
       (water_path area2 area14)
       (water_path area14 area2)
       (ground_path area1 area3)
       (ground_path area3 area1)
       (ground_path area3 area4)
       (ground_path area4 area3)
       (ground_path area3 area5)
       (ground_path area5 area3)
       (ground_path area4 area6)
       (ground_path area6 area4)
       (ground_path area4 area10)
       (ground_path area10 area4)
       (ground_path area10 area7)
       (ground_path area7 area10)
       (ground_path area7 area8)
       (ground_path area8 area7)
       (ground_path area5 area11)
       (ground_path area11 area5)
       (ground_path area11 area9)
       (ground_path area9 area11)
       (ground_path area10 area13)
       (ground_path area13 area10)
       (ground_path area3 area15)
       (ground_path area15 area3)
       (visible_from disaster1 area8)
       (visible_from disaster2 area12)
       (visible_from disaster3 area10)
       (visible_from disaster4 area7)
       (visible_from disaster5 area3)
       (in_range area2 area1)
       (in_range area3 area1)
       (in_range area1 area1)
       (in_range area9 area13)
       (in_range area10 area13)
       (in_range area13 area13))
  (:goal (and 
           (communicated_data disaster1)
           (communicated_data disaster2)
           (communicated_data disaster3)
           (communicated_data disaster4)
           (communicated_data disaster5)
           (box_at_area box1 area9)
           (box_at_area box2 area11)
           (have_water_sample_cdm cdm2 area8)
           (have_water_sample_cdm cdm1 area9))))