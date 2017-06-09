//calculate_bid(List,List2,Bid,Bid,NumberOfActions,NumberOfActions,Depth,Depth,Width,Width) :- timeout.
calculate_bid([],[],Bid,Bid,NumberOfActions,NumberOfActions,Depth,Depth,Width,Width).
calculate_bid([Plan|List],List2,Bid,BidR,NumberOfActions,NumberOfActionsR,Depth,DepthR,CurrentWidth,Width) :- jia.decomposeBody(Plan,Goals,Actions) & count_actions(Actions,NumberOfActions,NumberOfActionsAux) & decompose_goals(Goals,List,List2,Bid,BidR,NumberOfActionsAux,NumberOfActionsR,Depth,DepthR,CurrentWidth,Width).
calculate_bid([],List2,Bid,BidR,NumberOfActions,NumberOfActionsR,Depth,DepthR,CurrentWidth,Width) :- .length(List2,NewWidth) & (((NewWidth > CurrentWidth) & calculate_bid(List2,[],Bid,BidR,NumberOfActions,NumberOfActionsR,Depth+1,DepthR,NewWidth,Width)) | calculate_bid(List2,[],Bid,BidR,NumberOfActions,NumberOfActionsR,Depth+1,DepthR,CurrentWidth,Width)).


decompose_goals([],List,List2,Bid,BidR,NumberOfActions,NumberOfActionsR,Depth,DepthR,CurrentWidth,Width) :-  calculate_bid(List,List2,Bid+1,BidR,NumberOfActions,NumberOfActionsR,Depth,DepthR,CurrentWidth,Width).
decompose_goals(Goals,List,List2,Bid,BidR,NumberOfActions,NumberOfActionsR,Depth,DepthR,CurrentWidth,Width) :-  all_relevant(Goals,[],PList) & .concat(List2,PList,AuxList) & calculate_bid(List,AuxList,Bid+1,BidR,NumberOfActions,NumberOfActionsR,Depth,DepthR,CurrentWidth,Width).
