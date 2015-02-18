-module(zombie_fsm).
-author("Robert Hales rsjh3@kent.ac.uk").
-define(AIMLESS_STAY_COURSE, 8).

% Record to Props List, for reporting
-define(R2P(Record), record_to_propslist(#state{} = Record) ->
            lists:zip(record_info(fields, Record), tl(tuple_to_list(Record)))).

-include_lib("include/swarmer.hrl").
-behaviour(gen_fsm).

%gen_fsm implementation
-export([code_change/4,handle_event/3,handle_sync_event/4,
		 handle_info/3,init/1,terminate/3]).

-export([start_link/9,aimless/2,initial/2,aimless_search/2,active/2,
         active_search/2,chasing/2,chasing_search/2,calc_state/1,
         calc_aimlessbearing/3,start/1,pause/2,get_surroundings/2,
		 find_visible/2,find_visible/3]).

%API
-export([get_state/1, pause/1, unpause/1]).

-record(state, {id,
                tile,
                tile_size,
                num_columns,
                num_rows,
                viewer,
                viewerStr,
                speed,
                bearing,
                x,
                y,
                type,
                paused_state}).

start_link(X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,Bearing) -> 
	gen_fsm:start_link(?MODULE,[X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,Bearing],[]).


%%% API!
start(Pid) ->
    gen_fsm:send_event(Pid,start).

pause(Pid) ->
    gen_fsm:send_all_state_event(Pid, pause).

unpause(Pid) -> 
    gen_fsm:send_event(Pid,unpause).

get_state(Pid) ->
    catch gen_fsm:sync_send_all_state_event(Pid, get_state).

init([X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,_Bearing]) ->
	random:seed(erlang:now()),
    tile:summon_entity(Tile,{self(),{X,Y}, zombie}),
	{ok,initial,#state{id = list_to_binary(pid_to_list(self())), tile = Tile,viewer = Viewer, x = X, y = Y,speed=Speed, 
                       bearing=random:uniform(360),type =zombie,
                       viewerStr = list_to_binary(pid_to_list(Viewer)),
                       tile_size = TileSize, num_columns = NumColumns, 
                       num_rows = NumRows}}.

%%%%%%==========================================================================
%%%%%% State Machine
%%%%%%==========================================================================

initial(start,State) ->
    gen_fsm:send_event_after(State#state.speed, move),
	{next_state,calc_state(aimless),State}.
	
aimless(move,#state{speed = Speed, x = X, y = Y, tile_size = TileSize,
                    num_columns = NumColumns, num_rows = NumRows,
                    tile = Tile, type = Type} = State) ->
    OldBearing = State#state.bearing,
    StaySame = random:uniform(?AIMLESS_STAY_COURSE),
    Bearing = case StaySame of
        1 ->
            random:uniform(360);
        _ ->
            OldBearing
    end,
    {NewX, NewY} = calc_aimlessbearing(Bearing,X,Y),
    case (NewX < 0) or (NewY < 0) or (NewX > NumColumns * (TileSize-1)) or (NewY > NumRows * (TileSize-1)) of
        true -> % We are off the screen!
            {stop, shutdown, State};
        false ->
            NewTile = 
            % This calculates if the human is still in it's initial tile
            case {trunc(X) div TileSize, trunc(NewX) div TileSize, trunc(Y) div TileSize, trunc(NewY) div TileSize} of
                {XTile, XTile, YTile, YTile} -> % In same tile
                    Tile;
                {_, NewXTile, _, NewYTile} ->
                    tile:remove_entity(Tile, self(), Type),
                    list_to_atom("tile" ++  "X" ++ integer_to_list(NewXTile) ++  "Y" ++ integer_to_list(NewYTile))
            end,
            {ReturnedX,ReturnedY} = tile:update_entity(NewTile,{self(),{X,Y},Type},{NewX, NewY},Bearing,Speed),
            gen_fsm:send_event_after(State#state.speed, move),
            {next_state,aimless_search,State#state{x=ReturnedX,y=ReturnedY,bearing = Bearing, tile = NewTile}}
    end.

aimless_search(move,State) ->
	%get_surroundings(self(),State),
    gen_fsm:send_event_after(State#state.speed, move),
	{next_state,calc_state(aimless),State}.

active(move,State) ->
    gen_fsm:send_event_after(State#state.speed, move),
	{next_state,active_search,State}.

active_search(move,State) ->
    gen_fsm:send_event_after(State#state.speed, move),
	{next_state,calc_state(active),State}.
	
chasing(move,State) ->
    gen_fsm:send_event_after(State#state.speed, move),
	{next_state,chasing_search,State}.

chasing_search(move,State) ->
    gen_fsm:send_event_after(State#state.speed, move),
	{next_state,calc_state(chasing),State}.

pause(move, State) -> 
    %% If we get a move event start the timer again but don't actually move
    %% Ensure we will move after unpause.
    gen_fsm:send_event_after(State#state.speed, move),
    {next_state, pause, State};
pause(unpause, #state{paused_state = PausedState} = State) ->
	{next_state,PausedState,State}.

%Events for fsm.	
get_surroundings(Pid,#state{viewer=Viewer} = State) ->
	Map = viewer:get_population(Viewer),
		case maps:size(Map) of
			0-> [];
			_ -> Surroundings = maps:values(Map),
					find_visible(Surroundings,State)
		end.
		
calc_state(_Current_state) ->
	aimless.

find_visible(All,State) ->
	Visible = [],
	find_visible(All,State,Visible).
find_visible([],State,Visible) ->
	Visible;
find_visible([[{Pid,{Otherx,Othery}}]|Tail],State,Visible) ->
	error_logger:error_report(Pid),
	[].	
calc_aimlessbearing(Rand,X,Y) ->
	trigstuff:findcoordinates(Rand,X,Y).
%stuff for gen_fsm.
terminate(_,_StateName, #state{tile = Tile, type = Type} = _StateData) ->
    tile:remove_entity(Tile, self(), Type),
	ok.
code_change(_,StateName,StateData,_) ->
	{ok,StateName,StateData}.
handle_info(_,StateName,StateData)->
	{ok,StateName,StateData}.

handle_event(pause, StateName, StateData) ->
    {next_state,pause,StateData#state{paused_state = StateName}}.

handle_sync_event(get_state, _From, StateName, StateData) ->
    PropList = record_to_proplist(StateData),
    PropListJson = proplists:delete(viewer,PropList),
    {reply, {ok,PropListJson}, StateName,StateData}.

record_to_proplist(#state{} = Record) ->
    lists:zip(record_info(fields, state), tl(tuple_to_list(Record))).

