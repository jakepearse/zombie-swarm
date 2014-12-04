-module(zombie_fsm).
-author("Robert Hales rsjh3@kent.ac.uk").
-define(AIMLESS_STAY_COURSE, 8).
-behaviour(gen_fsm).

%gen_fsm implementation
-export([code_change/4,handle_event/3,handle_sync_event/4,handle_info/3,init/1,terminate/3]).

-export([start_link/7,aimless/2,initial/2,aimless_search/2,active/2,
         active_search/2,chasing/2,chasing_search/2,calc_state/1,
         calc_aimlessbearing/4,start/1,unpause/1,pause/2]).

%API
-export([get_position/1]).

-record(state, {id,tile,viewer,speed,bearing,x,y,timeoutz,type}).

start_link(X,Y,Tile,Viewer,Speed,Bearing,Timeout) -> 
	gen_fsm:start_link(?MODULE,[X,Y,Tile,Viewer,Speed,Bearing,Timeout],[]).

get_position(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, get_position).

init([X,Y,Tile,Viewer,Speed,Bearing,Timeout]) ->
	random:seed(erlang:now()),
	{ok,initial,#state{tile = Tile,viewer = Viewer, x = X, y = Y,speed=Speed, bearing=random:uniform(360), timeoutz=Timeout,type =zombie}}.



%States of fsm.	

initial(start,State) ->
    gen_fsm:send_event_after(State#state.timeoutz, move),
	{next_state,calc_state(aimless),State}.
	
aimless(move,State) ->
	OldBearing = State#state.bearing,
	StaySame = random:uniform(?AIMLESS_STAY_COURSE),
	Bearing = case StaySame of
		1 ->
			random:uniform(360);
		_ ->
			OldBearing
	end,
	P = calc_aimlessbearing(Bearing,State#state.speed,State#state.x,State#state.y),
	{NewX,NewY} = tile:update_entity(State#state.tile,{self(),{State#state.x,State#state.y}},P,Bearing,State#state.speed),
    gen_fsm:send_event_after(State#state.timeoutz, move),
	{next_state,aimless_search,State#state{x=NewX,y=NewY,bearing = Bearing}}.

aimless_search(move,State) ->
    gen_fsm:send_event_after(State#state.timeoutz, move),
	{next_state,calc_state(aimless),State}.

active(move,State) ->
    gen_fsm:send_event_after(State#state.timeoutz, move),
	{next_state,active_search,State}.

active_search(move,State) ->
    gen_fsm:send_event_after(State#state.timeoutz, move),
	{next_state,calc_state(active),State}.
	
chasing(move,State) ->
    gen_fsm:send_event_after(State#state.timeoutz, move),
	{next_state,chasing_search,State}.

chasing_search(move,State) ->
    gen_fsm:send_event_after(State#state.timeoutz, move),
	{next_state,calc_state(chasing),State}.
pause(unpause,State) ->
    gen_fsm:send_event_after(State#state.timeoutz, move),
	{next_state,aimless,State}.
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
terminate(shutdown,_StateName,_StateData) ->
	ok.
code_change(_,StateName,StateData,_) ->
	{ok,StateName,StateData}.
handle_info(_,StateName,StateData)->
	{ok,StateName,StateData}.
handle_event(_,_,_) ->
ok.

handle_sync_event(get_position, From, StateName, #state{x = X, y = Y} = StateData) ->
    {reply,{X, Y},StateName,StateData}.

%% Notes

% method of moving (dumb movement)
% 	decide on a bearing, based on prob
%	ask the viewer if it can go there
% 		if yes, go there
% 		if no, redecide 

%% Possible deadlock with this method... Collision detection

