-module(zombie_fsm).
-author("Robert Hales rsjh3@kent.ac.uk").
-define(AIMLESS_STAY_COURSE, 8).
-behaviour(gen_fsm).

%gen_fsm implementation
-export([code_change/4,handle_event/3,handle_sync_event/4,handle_info/3,init/1,terminate/3]).

-export([start_link/7,aimless/2,initial/2,aimless_search/2,active/2,active_search/2,chasing/2,chasing_search/2,calc_state/1,calc_aimlessbearing/4,start/1,unpause/1,pause/2]).
-record(state, {id,tile,viewer,speed,bearing,x,y,timeoutz,type}).

start_link(X,Y,Tile,Viewer,Speed,Bearing,Timeout) -> 
	gen_fsm:start_link(?MODULE,[X,Y,Tile,Viewer,Speed,Bearing,Timeout],[]).

init([X,Y,Tile,Viewer,Speed,Bearing,Timeout]) ->
	random:seed(erlang:now()),
	{ok,initial,#state{tile = Tile,viewer = Viewer, x = X, y = Y,speed=Speed, bearing=random:uniform(360), timeoutz=Timeout,type =zombie}}.

%States of fsm.	

initial(start,State) ->
	{next_state,calc_state(aimless),State,State#state.timeoutz}.
	
aimless(timeout,State) ->
	OldBearing = State#state.bearing,
	StaySame = random:uniform(?AIMLESS_STAY_COURSE),
	Bearing = case StaySame of
		1 ->
			random:uniform(360);
		_ ->
			OldBearing
	end,
	P = calc_aimlessbearing(Bearing,State#state.speed,State#state.x,State#state.y),
	case tile:update_entity(State#state.tile,{self(),{State#state.x,State#state.y}},P,Bearing,State#state.speed) of
		{_, ok, you_dead} ->
			{stop, normal, State};
		{{NewX,NewY},Tile,Viewer} ->
			{next_state,aimless_search,State#state{x=NewX,y=NewY,bearing = Bearing, tile = Tile, viewer = Viewer},State#state.timeoutz}
	end.

aimless_search(timeout,State) ->
	{next_state,calc_state(aimless),State,State#state.timeoutz}.

active(timeout,State) ->
	{next_state,active_search,State,State#state.timeoutz}.

active_search(timeout,State) ->
	{next_state,calc_state(active),State,State#state.timeoutz}.
	
chasing(timeout,State) ->
	{next_state,chasing_search,State,State#state.timeoutz}.

chasing_search(timeout,State) ->
	{next_state,calc_state(chasing),State,State#state.timeoutz}.
pause(unpause,State) ->
	{next_state,aimless,State,State#state.timeoutz}.
%Events for fsm.	
start(Pid) ->
	gen_fsm:send_event(Pid,start).
unpause(Pid) -> 
	gen_fsm:send_event(Pid,unpause).
calc_state(_Current_state) ->
	aimless.

calc_aimlessbearing(Rand,Speed,X,Y) ->
	trigstuff:findcoordinates(Rand,Speed,X,Y).
%stuff for gen_fsm.
terminate(_,_StateName,_StateData) ->
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
