{ include("$jacamoJar/templates/common-cartago.asl") }
{ include("$jacamoJar/templates/common-moise.asl") }
{ include("decomp_breadth.asl") }

+!create_taskboard
<-
	makeArtifact("task_board","TaskBoard",[]);
	.print("Created taskboard.");
	.
	
+!create_goal
<-
	.wait(50);
	announce(get_picture(flood1),5000,CNPBoardName);
	.print("Announced: get_picture(flood1) on ",CNPBoardName);
	getBids(Bids)[artifact_name(CNPBoardName)];
	.stopMAS;
	.