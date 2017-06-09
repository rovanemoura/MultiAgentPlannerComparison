{ include("$jacamoJar/templates/common-cartago.asl") }
{ include("$jacamoJar/templates/common-moise.asl") }
{ include("decomp_breadth.asl") }
//{ include("decomp_depth.asl") }
	
+!navigate(Area)
	: at(From) & .my_name(Name) & jcm__art(_,Name,Id)
<-
	visit(From)[artifact_id(Id)];
	!navigate(From,Area);
	unvisit(From)[artifact_id(Id)].

+!navigate(From,To)
	: at(To) .
	
+!navigate(From,To)
	: ground_path(From,To) & .my_name(Name) & jcm__art(_,Name,Id)
<-
	navigate(To)[artifact_id(Id)].
	
+!navigate(From,To)
	: ground_path(From,Mid) & (not visited(Mid)) & .my_name(Name) & jcm__art(_,Name,Id)
<-
	!navigate(From,Mid);
	visit(Mid)[artifact_id(Id)];
	!navigate(Mid,To);
	unvisit(Mid)[artifact_id(Id)].
	
+!get_picture(Flood)
	: visible_from(Flood,Area) & .my_name(Name) & jcm__art(_,Name,Id)
<-
	!navigate(Area);
	take_picture(Area,Flood)[artifact_id(Id)].
	
+!deliver_kit(AreaOrigin,AreaDestination)
	: .my_name(Name) & jcm__art(_,Name,Id)
<-
	!navigate(Area);
	get_kit[artifact_id(Id)];
	!navigate(Area);
	drop_kit[artifact_id(Id)].	