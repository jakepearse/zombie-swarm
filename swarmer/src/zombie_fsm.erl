-module(zombie_fsm).
-author("Robert Hales rsjh3@kent.ac.uk").
-behaviour(gen_fsm).

%gen_fsm implementation
-export([code_change/4,handle_event/3,handle_sync_event/4,handle_info/3,init/1,terminate/3]).

-export([start_link/7,update/1,aimless/2,initial/2,aimless_search/2,active/2,active_search/2,chasing/2,chasing_search/2,calc_state/1]).
-record(state, {id,tile,viewer,speed,bearing,x,y,timeout}).

start_link(X,Y,Tile,Viewer,Speed,Bearing,Timeout) -> 
	gen_fsm:start_link(?MODULE,[X,Y,Tile,Viewer,Speed,Bearing,Timeout],[]).

init([X,Y,Tile,Viewer,Speed,Bearing,Timeout]) ->
	{ok,initial,#state{tile = Tile,viewer = Viewer, x = X, y = Y,speed=Speed, bearing=Bearing, timeout=Timeout},#state.timeout}.

%States of fsm.	

initial(timeout,State) ->
	{next_state,calc_state(aimless),State,State#state.timeout}.
	
aimless(timeout,State) ->
	Bearing = random:uniform(360),
	{Changex,Changey} = calc_aimlessbearing(Bearing),
	{X,Y} = {State#state.x + Changex,State#state.y + Changey},
	{NewX,NewY} = tile:update_entity(State#state.tile,{self(),{State#state.x,State#state.y}},{X,Y},Bearing,State#state.speed),
	{next_state,aimless_search,State#state{x=NewX,y=NewY,bearing = Bearing},State#state.timeout}.

aimless_search(timeout,State) ->
	{next_state,calc_state(aimless),State,State#state.timeout}.

active(timeout,State) ->
	{next_state,active_search,State,State#state.timeout}.

active_search(timeout,State) ->
	{next_state,calc_state(active),State,State#state.timeout}.
	
chasing(timeout,State) ->
	{next_state,chasing_search,State,State#state.timeout}.

chasing_search(timeout,State) ->
	{next_state,calc_state(chasing),State,State#state.timeout}.

%Events for fsm.	
update([Newx,Newy,Pid]) ->
	gen_fsm:send_event(self(),{update,[Newx,Newy,Pid]}).

calc_state(_Current_state) ->
	aimless.

calc_aimlessbearing(_rand) ->
	{1,0}.
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

%% Notes

% method of moving (dumb movement)
% 	decide on a bearing, based on prob
%	ask the viewer if it can go there
% 		if yes, go there
% 		if no, redecide 

%% Possible deadlock with this method... Collision detection
