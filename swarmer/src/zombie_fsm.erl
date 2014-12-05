-module(zombie_fsm).
-author("Robert Hales rsjh3@kent.ac.uk").
-define(AIMLESS_STAY_COURSE, 8).

-include_lib("include/swarmer.hrl").
-behaviour(gen_fsm).

%gen_fsm implementation
-export([code_change/4,handle_event/3,handle_sync_event/4,
		 handle_info/3,init/1,terminate/3]).

-export([start_link/10,aimless/2,initial/2,aimless_search/2,active/2,
         active_search/2,chasing/2,chasing_search/2,calc_state/1,
         calc_aimlessbearing/4,start/1,pause/2]).

%API
-export([get_state/1, pause/1, unpause/1]).

-record(state, {id,
                tile,
                tile_size,
                num_columns,
                num_rows,
                viewer,
                speed,
                bearing,
                x,
                y,
                timeoutz,
                type,
                paused_state}).

start_link(X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,Bearing,Timeout) -> 
	gen_fsm:start_link(?MODULE,[X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,Bearing,Timeout],[]).


%%% API!
start(Pid) ->
    gen_fsm:send_event(Pid,start).

pause(Pid) ->
    gen_fsm:send_event(Pid, pause).

unpause(Pid) -> 
    gen_fsm:send_event(Pid,unpause).

get_state(Pid) ->
    catch gen_fsm:sync_send_all_state_event(Pid, get_state).

init([X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,_Bearing,Timeout]) ->
	random:seed(erlang:now()),
    tile:summon_entity(Tile,{self(),{X,Y}}),
	{ok,initial,#state{tile = Tile,viewer = Viewer, x = X, y = Y,speed=Speed, 
                       bearing=random:uniform(360), timeoutz=Timeout,type =zombie,
                       tile_size = TileSize, num_columns = NumColumns, 
                       num_rows = NumRows}}.

%%%%%%==========================================================================
%%%%%% State Machine
%%%%%%==========================================================================

initial(start,State) ->
    gen_fsm:send_event_after(State#state.timeoutz, move),
	{next_state,calc_state(aimless),State}.
	
aimless(move,#state{speed = Speed, x = X, y = Y, tile_size = TileSize,
                    num_columns = NumColumns, num_rows = NumRows,
                    tile = Tile} = State) ->
	OldBearing = State#state.bearing,
	StaySame = random:uniform(?AIMLESS_STAY_COURSE),
	Bearing = case StaySame of
		1 ->
			random:uniform(360);
		_ ->
			OldBearing
	end,
	{NewX, NewY} = calc_aimlessbearing(Bearing,Speed,X,Y),
    case (NewX < 0) or (NewY < 0) or (NewX > NumColumns * TileSize) or (NewY > NumRows * TileSize) of
        true -> % We are off the screen!
            {stop, shutdown, State};
        false ->
            NewTile = 
            % This calculates if the zombie is still in it's initial tile
            case {trunc(X) div TileSize, trunc(NewX) div TileSize, trunc(Y) div TileSize, trunc(NewY) div TileSize} of
                {XTile, XTile, YTile, YTile} -> % In same tile
                    Tile;
                {_, NewXTile, _, NewYTile} ->
                    tile:remove_entity(Tile, self()),
                    list_to_atom("tile" ++  "X" ++ integer_to_list(NewXTile) ++  "Y" ++ integer_to_list(NewYTile))
            end,
            {ReturnedX,ReturnedY} = tile:update_entity(NewTile,{self(),{X,Y}},{NewX, NewY},Bearing,Speed),
            gen_fsm:send_event_after(State#state.timeoutz, move),
            {next_state,aimless_search,State#state{x=ReturnedX,y=ReturnedY,bearing = Bearing, tile = NewTile}}
    end.

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

pause(move, State) -> 
    %% If we get a move event start the timer again but don't actually move
    %% Ensure we will move after unpause.
    gen_fsm:send_event_after(State#state.timeoutz, move),
    {next_state, pause, State};
pause(unpause, #state{paused_state = PausedState} = State) ->
	{next_state,PausedState,State}.

%Events for fsm.	

calc_state(_Current_state) ->
	aimless.
	
calc_aimlessbearing(Rand,Speed,X,Y) ->
	trigstuff:findcoordinates(Rand,Speed,X,Y).
%stuff for gen_fsm.
terminate(_,_StateName, #state{tile = Tile} = _StateData) ->
    tile:remove_entity(Tile, self()),
	ok.
code_change(_,StateName,StateData,_) ->
	{ok,StateName,StateData}.
handle_info(_,StateName,StateData)->
	{ok,StateName,StateData}.

handle_event(pause, StateName, StateData) ->
    {next_state,pause,StateData#state{paused_state = StateName}}.

handle_sync_event(get_state, _From, StateName, 
                  #state{x = X, y = Y, speed = Speed, type = Type,
                         bearing = Bearing} = StateData) ->
    {reply,{ok,#entity_status{id = self(), x = X, y = Y, type = Type, 
                              current_activity = StateName, speed = Speed,
                              bearing = Bearing}},StateName,StateData}.

