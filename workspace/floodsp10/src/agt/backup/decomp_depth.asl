count_actions([],NumberOfActions,NumberOfActions).
count_actions([Action|ActionList],NumberOfActions,NumberOfActionsAux) :- count_actions(ActionList,NumberOfActions+1,NumberOfActionsAux).

all_relevant([],PList,PList).
all_relevant([Goal|GList],PList,AuxList) :- .relevant_plans({+!Goal},Plans)  & .concat(PList,Plans,Aux)  & all_relevant(GList,Aux,AuxList).

!start.

+!start <- 	focusWhenAvailable("task_board");
	.print("Task board located.").
	
+task(Task,CNPBoard)
<- 
    !!timeout(1000);
  focusWhenAvailable(CNPBoard);
  .print("New task announced: ",Task);
  .relevant_plans({+!Task},Plans);
  jia.convert(Plans,Plans2);
  .length(Plans2,CurrentWidth);
  .nth(0,Plans2,Plan);
  .delete(0,Plans2,List);
  !calculate_bid(Plan,List,0,0,0,0,CurrentWidth);
  .
  
+!calculate_bid(Plan,List,Bid,NumberOfActions,Depth,MaxDepth,Width)
	: timeout
<-
	.print("Timeout expired.");
	.print("Number of plans: ",Bid);
    .print("Number of actions: ",NumberOfActions);
    .print("Plantree max depth: ",MaxDepth);
    .print("Plantree max width: ",Width);
	.

+!calculate_bid([],[],Bid,NumberOfActions,Depth,MaxDepth,Width)
<-
	.print("Number of plans: ",Bid);
    .print("Number of actions: ",NumberOfActions);
    .print("Plantree max depth: ",MaxDepth);
    .print("Plantree max width: ",Width);
    .

+!calculate_bid(Plan,List,Bid,NumberOfActions,Depth,MaxDepth,CurrentWidth)
	: jia.decomposeBody(Plan,Goals,Actions) & count_actions(Actions,NumberOfActions,NumberOfActionsR)
<-
	!decompose_goals(Goals,List,Bid,NumberOfActionsR,Depth,MaxDepth,CurrentWidth).

+!decompose_goals([],List,Bid,NumberOfActions,Depth,MaxDepth,CurrentWidth)
	: Depth > MaxDepth
<-
	.nth(0,List,Plan);
	.delete(0,List,List2);
	!calculate_bid(Plan,List2,Bid+1,NumberOfActions,Depth-1,Depth,CurrentWidth).
	
+!decompose_goals([],List,Bid,NumberOfActions,Depth,MaxDepth,CurrentWidth)
<-
	.nth(0,List,Plan);
	.delete(0,List,List2);
	!calculate_bid(Plan,List2,Bid+1,NumberOfActions,Depth-1,MaxDepth,CurrentWidth).

+!decompose_goals(Goals,List,Bid,NumberOfActions,Depth,MaxDepth,CurrentWidth) 
:  all_relevant(Goals,[],PList) & .length(PList,NewWidth) & NewWidth > CurrentWidth & .concat(PList,List,AuxList) 
<-
	.nth(0,AuxList,Plan);
	.delete(0,AuxList,List2);
	!calculate_bid(Plan,List2,Bid+1,NumberOfActions,Depth+1,MaxDepth,NewWidth).
	
+!decompose_goals(Goals,List,Bid,NumberOfActions,Depth,MaxDepth,CurrentWidth) 
:  all_relevant(Goals,[],PList) & .concat(PList,List,AuxList) 
<-
	.nth(0,AuxList,Plan);
	.delete(0,AuxList,List2);
	!calculate_bid(Plan,List2,Bid+1,NumberOfActions,Depth+1,MaxDepth,CurrentWidth).

  
+!timeout(Time)
<-
	.wait(Time);
	+timeout;
	.