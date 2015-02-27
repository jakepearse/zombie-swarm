-module(human_fsm).
-author("Joe Mitchard jm710").

-define(AIMLESS_STAY_COURSE, 8).
-define(SIGHT,100).
-define(PERSONAL_SPACE, 3).
-define(LIMIT,5).

-include_lib("include/swarmer.hrl").
-behaviour(gen_fsm).

%gen_fsm implementation
-export([code_change/4,handle_event/3,handle_sync_event/4,
         handle_info/3,init/1,terminate/3]).

-export([start_link/10,aimless/2,initial/2,aimless_search/2,active/2,
         active_search/2,chasing/2,chasing_search/2,calc_state/1,
         calc_aimlessbearing/3,start/1,pause/2]).

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
                timeout,
                type,
                paused_state,
                x_velocity,
                y_velocity}).

start_link(X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,Bearing,Timeout) -> 
    gen_fsm:start_link(?MODULE,[X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,Bearing,Timeout],[]).


%%% API!
start(Pid) ->
    gen_fsm:send_event(Pid,start).

pause(Pid) ->
    gen_fsm:send_all_state_event(Pid, pause).

unpause(Pid) -> 
    gen_fsm:send_event(Pid,unpause).

get_state(Pid) ->
    catch gen_fsm:sync_send_all_state_event(Pid, get_state).

init([X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,_Bearing,Timeout]) ->
    random:seed(erlang:now()),
    tile:summon_entity(Tile,{self(),{X,Y}, human}),
    {ok,initial,#state{id = list_to_binary(pid_to_list(self())),
                       tile = Tile,viewer = Viewer, x = X, y = Y,speed=Speed, 
                       bearing=random:uniform(360), timeout=Timeout,type =human,
                       viewerStr = list_to_binary(pid_to_list(Viewer)),
                       tile_size = TileSize, num_columns = NumColumns, 
                       num_rows = NumRows,
                       x_velocity = 0, y_velocity = 0}}.

%%%%%%==========================================================================
%%%%%% State Machine
%%%%%%==========================================================================

initial(start,State) ->
    gen_fsm:send_event_after(State#state.timeout, move),
    {next_state,calc_state(aimless),State}.
    
aimless(move,#state{speed = Speed, x = X, y = Y, tile_size = TileSize,
                    num_columns = NumColumns, num_rows = NumRows,
                    tile = Tile, type = Type,
                    x_velocity = X_Velocity, y_velocity = Y_Velocity} = State) ->

    % Build a list of nearby zombies
    Zombie_List = viewer:get_zombies(State#state.viewer),
    Zlist = lists:keysort(1,lists:map(fun({ZomPid,{ZType,{{ZX,ZY},{ZX_Velocity,ZY_Velocity}}}}) ->
        {pythagoras:pyth(X,Y,ZX,ZY),{ZomPid,{ZType,{{ZX,ZY},{ZX_Velocity,ZY_Velocity}}}}} end,
        Zombie_List)),


    % Build a list of nearby humans
    Human_List = viewer:get_humans(State#state.viewer),
    No_self_list = lists:keydelete(self(),1,Human_List),
    DistanceList = lists:map(fun({Hpid,{human,{{HX,HY},{HXV,HYV}}}}) -> 
        {pythagoras:pyth(X,Y,HX,HY),{Hpid,{human,{{HX,HY},{HXV,HYV}}}}} end,
        No_self_list),


    Hlist = lists:keysort(1,DistanceList),


    Zlist_Json = jsonify_list(Zlist),
    Hlist_Json = jsonify_list(Hlist),


    {BoidsX,BoidsY} = make_choice(Hlist,Zlist,State), 

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
            {ReturnedX,ReturnedY} = tile:update_entity(NewTile,{self(),{X,Y}, Type},{NewX, NewY},Bearing,Speed, {X_Velocity, Y_Velocity}),
            gen_fsm:send_event_after(State#state.timeout, move),
            {next_state,aimless_search,State#state{x=ReturnedX,y=ReturnedY,bearing = Bearing, tile = NewTile, , z_list = Zlist_Json, h_list = Hlist_Json}}
    end.

aimless_search(move,State) ->
    gen_fsm:send_event_after(State#state.timeout, move),
    {next_state,calc_state(aimless),State}.

active(move,State) ->
    gen_fsm:send_event_after(State#state.timeout, move),
    {next_state,active_search,State}.

active_search(move,State) ->
    gen_fsm:send_event_after(State#state.timeout, move),
    {next_state,calc_state(active),State}.
    
chasing(move,State) ->
    gen_fsm:send_event_after(State#state.timeout, move),
    {next_state,chasing_search,State}.

chasing_search(move,State) ->
    gen_fsm:send_event_after(State#state.timeout, move),
    {next_state,calc_state(chasing),State}.

pause(move, State) -> 
    %% If we get a move event start the timer again but don't actually move
    %% Ensure we will move after unpause.
    gen_fsm:send_event_after(State#state.timeout, move),
    {next_state, pause, State};
pause(unpause, #state{paused_state = PausedState} = State) ->
    {next_state,PausedState,State}.

%Events for fsm.    

calc_state(_Current_state) ->
    aimless.
    
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


% BOIDS
make_choice([],[],_State) ->
    {0,0};

% Collision Avoidance
make_choice([{Dist, {_,{_,{{HeadX,HeadY},{_Head_X_Vel,_Head_Y_Vel}}}}}|_Hlist],_,State) when Dist < ?PERSONAL_SPACE ->
    boids_functions:collision_avoidance(State#state.x, State#state.y, HeadX, HeadY);

% Repulsor
make_choice(_,[{_Dist, {_,{_,{{HeadX,HeadY},{_Head_X_Vel,_Head_Y_Vel}}}}}|_Zlist],State) ->
    boids_functions:super_repulsor(State#state.x,State#state.y,HeadX,HeadY);

% Flock
make_choice(Hlist,_, State) ->
    {Fx,Fy} = boids_functions:flocking(Hlist,State#state.x,State#state.y),
    {Vx,Vy} = boids_functions:velocity(Hlist,State#state.x_velocity,State#state.y_velocity),
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
