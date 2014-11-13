-module(zombie_fsm).
-author("Robert Hales rsjh3@kent.ac.uk").
-behaviour(gen_fsm).

%gen_fsm implementation
-export([code_change/4,handle_event/3,handle_sync_event/4,handle_info/3,init/1,terminate/3]).

-export([start_link/4,update/1,aimless/2]).
-record(state, {tile,viewer,speed,direction,x,y}).

start_link(X,Y,Tile,Viewer) -> 
gen_fsm:start_link({local,?MODULE},?MODULE,[X,Y,Tile,Viewer],[]).

init([X,Y,Tile,Viewer]) ->
	{ok,aimless,#state{tile = Tile,viewer = Viewer, x = X, y = Y}}.

%States of fsm.	
aimless({update,[Newx,Newy,Pid]},State) ->
	{next_state,aimless,State#state{x=Newx,y=Newy,tile=Pid}}.

%Events for fsm.	
update([Newx,Newy,Pid]) ->
	gen_fsm:send_event(zombie_fsm,{update,[Newx,Newy,Pid]}).

	
%stuff for gen_fsm.
terminate(shutdown,_StateName,_StateData) ->
	ok.
code_change(_,StateName,StateData,_) ->
	{ok,StateName,StateData}.
handle_info(_,StateName,StateData)->
	{ok,StateName,StateData}.
handle_event(_,_,_) ->
ok.
handle_sync_event(_,_,_,_) ->
ok.
