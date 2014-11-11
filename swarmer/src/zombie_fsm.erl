-module(zombie_fsm).
-behaviour(gen_fsm).

%gen_fsm implementation
-export([code_change/4,handle_event/3,handle_sync_event/4,handle_info/3,init/1,terminate/3]).

-export([start_link/1,update/3,initial_state/2,aimless/2]).
-record(state, {tile,viewer,speed,direction,x,y}).

start_link(A) -> 
gen_fsm:start_link({local,?MODULE},?MODULE,A,[]).

init(A) ->
	{ok,initial_state,#state{tile=A}}.
		
initial_state({update,Newx,Newy,Pid},A) ->
	{next_state,aimless,A#state{x=Newx,y=Newy,tile=Pid}}.
aimless({update,Newx,Newy,Pid},A) ->
	{next_state,aimless,A#state{x=Newx,y=Newy,tile=Pid}}.
update(Newx,Newy,Pid) ->
	gen_fsm:send_event(zombie_fsm,{update,Newx,Newy,Pid}).

	
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
