-module(zombie_fsm).
-author("Robert Hales rsjh3@kent.ac.uk").
-behaviour(gen_fsm).

%gen_fsm implementation
-export([code_change/4,handle_event/3,handle_sync_event/4,handle_info/3,init/1,terminate/3]).

-export([start_link/4,update/1,aimless/2,initial/2,aimless_search/2,active/2,active_search/2,chasing/2,chasing_search/2,calc_state/1]).
-record(state, {tile,viewer,speed,direction,x,y}).

start_link(X,Y,Tile,Viewer) -> 
gen_fsm:start_link(?MODULE,[X,Y,Tile,Viewer],[]).

init([X,Y,Tile,Viewer]) ->
	{ok,initial,#state{tile = Tile,viewer = Viewer, x = X, y = Y},300}.

%States of fsm.	

initial(timeout,State) ->
	{next_state,calc_state(aimless),State,300}.
	
aimless(timeout,State) ->
	X = State#state.x,
	Y = State#state.y + 1,
%tile:update_entity(State#state.tile,{self(),{State#state.x,State#state.y}},{X,Y},n,10),
	{next_state,aimless_search,State#state{x=X,y=Y},300}.

aimless_search(timeout,State) ->
	{next_state,calc_state(aimless),State,300}.

active(timeout,State) ->
	{next_state,active_search,State,300}.

active_search(timeout,State) ->
	{next_state,calc_state(active),State,300}.
	
chasing(timeout,State) ->
	{next_state,chasing_search,State,300}.

chasing_search(timeout,State) ->
	{next_state,calc_state(chasing),State,300}.

%Events for fsm.	
update([Newx,Newy,Pid]) ->
	gen_fsm:send_event(self(),{update,[Newx,Newy,Pid]}).

calc_state(Current_state) ->
	aimless.
	
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
