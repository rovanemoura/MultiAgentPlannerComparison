{ include("planner.asl") }

have_soil_analysis(way1).

/* Initial beliefs and rules */
regra(V2,N2) :- soil_sample(V2)[artifact_id(I2)] & jcm__art(N2,I2).  

/* Initial goals */

!start.

/* Plans 

+!start : .findall(par(V2,N2),regra(V2,N2),L2) & .print(L2) <- for (soil_sample(V)[artifact_id(I)]) {
			?jcm__art(N,I);
			.print("Soil sample: ",V,"  in (artefact) ",N);
}.
*/

+!start
	: have_soil_analysis(Waypoint)
	<-	//jia.startPlanner;
		//.print(have_soil_analysis(Waypoint));
		//communicate_data[artifact_id(cobj_3)];
		//?channel(X)[artifact_id(Y)];
		.print("");
		.

+!send_rock_data
	: true
	<-	communicate_data(have_rock_analysis(Waypoint)).
	
+!send_rock_data
	: true
	<-	//!navigate;
		communicate_data(have_rock_analysis(Waypoint)).

+!communicate_image_data
	: have_image(Objective,Mode)
	<-	.print("");
		communicate_data(have_image(Objective,Mode)).
	