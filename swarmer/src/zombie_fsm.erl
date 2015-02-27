-module(zombie_fsm).
-author("Robert Hales rsjh3@kent.ac.uk").

-define(AIMLESS_STAY_COURSE, 8).
-define(SIGHT,75).
-define(PERSONAL_SPACE, 3).
-define(LIMIT,5).

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
                paused_state,
                x_velocity,
                y_velocity,
                z_list,
                h_list}).

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
	{ok,initial,#state{id = list_to_binary(pid_to_list(self())), 
                       tile = Tile,viewer = Viewer, x = X, y = Y,speed=Speed, 
                       bearing=random:uniform(360),type =zombie,
                       viewerStr = list_to_binary(pid_to_list(Viewer)),
                       tile_size = TileSize, num_columns = NumColumns, 
                       num_rows = NumRows,
                       x_velocity = 0, y_velocity = 0}}.

%%%%%%==========================================================================
%%%%%% State Machine
%%%%%%==========================================================================

initial(start,State) ->
    gen_fsm:send_event_after(State#state.speed, move),
	{next_state,calc_state(aimless),State}.
	
aimless(move,#state{speed = Speed, x = X, y = Y, tile_size = TileSize,
                    num_columns = NumColumns, num_rows = NumRows,
                    tile = Tile, type = Type,
                    x_velocity = X_Velocity, y_velocity = Y_Velocity} = State) ->
    % OldBearing = State#state.bearing,
    % StaySame = random:uniform(?AIMLESS_STAY_COURSE),
    % Bearing = case StaySame of
    %     1 ->
    %         random:uniform(360);
    %     _ ->
    %         OldBearing
    % end,
    % {NewX, NewY} = calc_aimlessbearing(Bearing,X,Y),


    % Build a list of nearby zombies
    ZombieList = viewer:get_zombies(State#state.viewer),

    NoSelfList = lists:keydelete(self(),1,ZombieList),

    Z_DistanceList = lists:map(fun(
                                {ZomPid,{ZType,{{ZX,ZY},{ZX_Velocity,ZY_Velocity}}}}) ->
                                    {pythagoras:pyth(X,Y,ZX,ZY),
                                    {ZomPid,{ZType,{{ZX,ZY},
                                    {ZX_Velocity,ZY_Velocity}}}}} 
                            end,NoSelfList),

    Z_FilteredList = lists:filter(
                                fun({Dist,{_,{_,{{_,_},{_,_}}}}}) ->
                                    Dist =< ?SIGHT
                                end,Z_DistanceList),

    Zlist = lists:keysort(1,Z_FilteredList),


    % Build a list of nearby humans
    HumanList = viewer:get_humans(State#state.viewer),

    H_DistanceList = lists:map(fun(
                                {Hpid,{human,{{HX,HY},{HXV,HYV}}}}) -> 
                                    {pythagoras:pyth(X,Y,HX,HY),
                                    {Hpid,{human,{{HX,HY},
                                    {HXV,HYV}}}}} 
                            end,HumanList),

    H_FilteredList = lists:filter(
                                fun({Dist,{_,{_,{{_,_},{_,_}}}}}) ->
                                    Dist =< ?SIGHT
                                end,H_DistanceList),

    Hlist = lists:keysort(1,H_FilteredList),


    Zlist_Json = jsonify_list(Zlist),
    Hlist_Json = jsonify_list(Hlist),


    {BoidsX,BoidsY} = make_choice(Zlist,Hlist,State), 
    
    New_X_Velocity = X_Velocity + BoidsX,
    New_Y_Velocity = Y_Velocity + BoidsY,
    {Limited_X_Velocity,Limited_Y_Velocity} = boids_functions:limit_speed(?LIMIT,X,Y,New_X_Velocity,New_Y_Velocity),
    NewX = X + Limited_X_Velocity,
    NewY = Y + Limited_Y_Velocity,  

    Bearing = 0,

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
            {ReturnedX,ReturnedY} = tile:update_entity(NewTile,{self(),{X,Y},Type},{NewX, NewY},Bearing,Speed, {New_X_Velocity, New_Y_Velocity}),
            gen_fsm:send_event_after(State#state.speed, move),
            {next_state,aimless_search,State#state{x=ReturnedX,y=ReturnedY,bearing = Bearing, tile = NewTile, z_list = Zlist_Json, h_list = Hlist_Json, x_velocity = Limited_X_Velocity,y_velocity = Limited_Y_Velocity}}
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
get_surroundings(_Pid,#state{viewer=Viewer} = State) ->
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
find_visible([],_State,Visible) ->
	Visible;
find_visible([[{Pid,{_Otherx,_Othery}}]|_Tail],_State,_Visible) ->
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

make_choice([],[],_State) ->
    {0,0};

%make_choice(_,[{Dist, {_,{_,{{_,_},{_,_}}}}}|_Hlist],State) when Dist < ?PERSONAL_SPACE ->
%    KILL HUMAN;

make_choice([{Dist, {_,{_,{{HeadX,HeadY},{_Head_X_Vel,_Head_Y_Vel}}}}}|_Zlist],_,State) when Dist < ?PERSONAL_SPACE ->
    boids_functions:collision_avoidance(State#state.x, State#state.y, HeadX, HeadY);

make_choice(_,[{_Dist, {_,{_,{{HeadX,HeadY},{_Head_X_Vel,_Head_Y_Vel}}}}}|_Hlist],State) ->
    boids_functions:super_attractor(State#state.x,State#state.y,HeadX,HeadY);

make_choice(Zlist,_, State) ->
    {Fx,Fy} = boids_functions:flocking(Zlist,State#state.x,State#state.y),
    {Vx,Vy} = boids_functions:velocity(Zlist,State#state.x_velocity,State#state.y_velocity),
    {(Fx+Vx),(Fy+Vy)}.


jsonify_list([]) ->
    [];
jsonify_list(List) ->
    jsonify_list(List,[]).

jsonify_list([], List) ->
    List;
jsonify_list([{Dist, {Pid,{Type,{{HeadX,HeadY},{Head_X_Vel,Head_Y_Vel}}}}}|Ls], List) ->
    StringPid = list_to_binary(pid_to_list(Pid)),
    NewList = [[{id, StringPid},{type, Type}, {dist, Dist}, {x, HeadX}, {y, HeadY}, {x_velocity, Head_X_Vel}, {y_velocity, Head_Y_Vel}]| List],
    jsonify_list(Ls, NewList).
