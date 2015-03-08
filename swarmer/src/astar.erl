-module(astar).
-export([findpath/3,nodeNeighbours/4]).

rebuildPath(A,B) ->

    B.

% generates a list of all neighbours of Node that are not in Obs_List
%nodeNeighbours([{_F,Parent,NodeG,{NodeX,NodeY}}],G,{GoalX,GoalY},Obs_List)->
  %%{NodeX,NodeY} = Node,
  %Node = {_F,Parent,G,{NodeX,NodeY}},
  %N=lists:filter(fun(Z) -> lists:any(fun(W) -> not (W==Z) end, Obs_List++{NodeX,NodeY})
  %end,lists:map(fun({_,_,_,{A,B}}) -> {pythagoras:pyth(A+NodeX,B+NodeY,GoalX,GoalY),{NodeX,NodeY},G+1,{A+NodeX,B+NodeY}} end,
  %[{a,b,c,{X,Y}} || X <- lists:seq(-1,1), Y <- lists:seq(-1,1)])),
  %error_logger:error_report(N),
  %N;

nodeNeighbours({_F,Parent,NodeG,{NodeX,NodeY}},G,{GoalX,GoalY},Obs_List)->
  %{NodeX,NodeY} = Node,
  Node = {_F,Parent,G,{NodeX,NodeY}},
  N=lists:filter(fun(Z) -> lists:any(fun(W) -> not (W==Z) end, Obs_List++{NodeX,NodeY})
  end,lists:map(fun({_,_,_,{A,B}}) -> {pythagoras:pyth(A+NodeX,B+NodeY,GoalX,GoalY),{NodeX,NodeY},G+1,{A+NodeX,B+NodeY}} end,
  [{a,b,c,{X,Y}} || X <- lists:seq(-1,1), Y <- lists:seq(-1,1)])),
  %error_logger:error_report(N),
  ordsets:union(N,[]).
  
findpath(Start,End,Obs_List)->
  {StartX,StartY}=Start,
  {GoalX,GoalY}=End,
  G = 0, % G(x) gives us the cost from Start to current node
  ClosedSet = ordsets:new(),
  % an element of the set looks like {F(x),ParentNode,G(x),{NodeCoordinates}}
  OpenSet = ordsets:add_element({pythagoras:pyth(StartX,StartY,GoalX,GoalY),none,G,Start},ordsets:new()),
  findpath(Start,End,Obs_List,ClosedSet,OpenSet,0).
  
  
  % put in a case where OpenSet is empty!
  
  findpath(Start,End,Obs_List,ClosedSet,OpenSet,G) ->
  {GoalX,GoalY}=End,
    [CurrentNode|Tail] = OpenSet,
    {_F,_Parent,_NodeG,NodeCoordinates} = CurrentNode,
    %NodeCoordinates = CurrentNode,
    %error_logger:error_report(NodeCoordinates),

    case NodeCoordinates == End of
      true ->
        rebuildPath(CurrentNode,ClosedSet);
      false ->
        
        NewClosedSet = ordsets:add_element(CurrentNode,ClosedSet),
        Neighbours = ordsets:union(
          lists:filter(fun(N)->
                        lists:any(fun(M)-> 
                                  not (N==M) end,NewClosedSet) end,
                                  nodeNeighbours(CurrentNode,G,End,Obs_List)
                        ),[]),
        NextOpenSet = ordsets:union(OpenSet,Neighbours),
        NewOpenSet = ordsets:del_element(CurrentNode,NextOpenSet),
          %{pythagoras:pyth(NX,NY,GoalX,GoalY),NodeCoordinates,G+1,{NX,NY}} end,
           % Neighbours)],
        findpath(Start,End,Obs_List,NewClosedSet,NewOpenSet,G+1)
      end.
      
        
      
  
  
  
  
