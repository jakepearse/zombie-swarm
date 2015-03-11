-module(human_fsm).
-author("Joe Mitchard jm710").

-define(RUN_STAY_COURSE, 8).
-define(SIGHT,100).
-define(PERSONAL_SPACE, 3).

%Variables for boids.
-define(LIMIT,5).
-define(TIRED_LIMIT,2).
-define(HUNGRY_LIMIT,4).
-define(SUPER_EFFECT, 0.3).
-define(FLOCKING_EFFECT,0.5).
-define(VELOCITY_EFFECT,0.5).
-define(COHESION_EFFECT,0.2).

% Behaviour Parameters
-define(INITIAL_HUNGER,5).
-define(INITIAL_ENERGY,100).
-define(HUNGRY_LEVEL, 25).
-define(TIRED_LEVEL, 25).

-include_lib("include/swarmer.hrl").
-behaviour(gen_fsm).

%gen_fsm implementation
-export([code_change/4,handle_event/3,handle_sync_event/4,
         handle_info/3,init/1,terminate/3]).

-export([start_link/10,run/2,initial/2,
        calc_state/1,calc_runbearing/3,start/1,pause/2]).

%API
-export([get_state/1, pause/1, unpause/1,zombify/1]).

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
                y_velocity,
                z_list,
                h_list,
                i_list,
                memory_list,
                hunger_state,
                hunger,
                energy,
                memory_map = maps:new()}).

start_link(X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,Bearing,Timeout) -> 
    gen_fsm:start_link(?MODULE,[X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,Bearing,Timeout],[]).


%%% API!
start(Pid) ->
    gen_fsm:send_event(Pid,start).

pause(Pid) ->
    gen_fsm:send_all_state_event(Pid, pause).

unpause(Pid) -> 
    gen_fsm:send_event(Pid,unpause).

zombify(Pid) ->
    gen_fsm:send_all_state_event(Pid, zombify).

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
                       x_velocity = 0, y_velocity = 0,
                       hunger = ?INITIAL_HUNGER, energy = ?INITIAL_ENERGY,
                       hunger_state = not_hungry}}.

%%%%%%==========================================================================
%%%%%% State Machine
%%%%%%==========================================================================

initial(start,State) ->
    gen_fsm:send_event_after(State#state.timeout, move),
    {next_state,calc_state(run),State}.
    
run(move,#state{speed = Speed, x = X, y = Y, tile_size = TileSize,
                    num_columns = NumColumns, num_rows = NumRows,
                    tile = Tile, type = Type,
                    x_velocity = X_Velocity, y_velocity = Y_Velocity,
                    viewer = Viewer,
                    hunger = Hunger, energy = Energy,
                    memory_map = MemoryMap} = State) ->

    % Build a list of nearby zombies
    Zlist = build_zombie_list(Viewer, X, Y),

    % Build a list of nearby humans
    Hlist = build_human_list(Viewer, X, Y),

    % Build a list of nearby items and store them to memory
    Ilist = viewer:get_items(Viewer),
    % NewMemoryMap = build_memory(Ilist, TileSize, MemoryMap),
    NewMemoryMap = build_memory(Ilist, MemoryMap),
    % error_logger:error_report(NewMemoryMap),

    Olist = viewer:get_obs(Viewer),


    Zlist_Json = jsonify_list(Zlist),
    Hlist_Json = jsonify_list(Hlist),

    % creates a new value for hunger and food, showing the humans getting
    % hungry over time
    {NewHunger, NewEnergy, NewHungerState} = case Hunger of
        HValue when HValue =< 0 ->
            case Energy of 
                EVAlue when EVAlue =< ?TIRED_LEVEL ->
                    {Hunger,Energy, tired};
                _ -> 
                    {Hunger, Energy-1, very_hungry}
            end;
        HValue when HValue =< ?HUNGRY_LEVEL ->
            {Hunger-1, Energy-1, hungry};
        _ ->
            {Hunger-1, Energy, not_hungry}
    end,

    NearestItem = get_nearest_item(Ilist,{X,Y}),

    % error_logger:error_report(NearestItem),

    %%%% NEEDS REFACTORING
    {BoidsX, BoidsY} = case NewHungerState of 
        tired ->
            % need to search for food, boids a little, but also limit speed
            MemoryList = maps:keys(NewMemoryMap),
            % error_logger:error_report(MemoryList),
            case make_choice(Hlist,Zlist, NearestItem, NewHungerState, State) of
                {BX,BY} ->  
                    % Local food! Go forth hungry human!
                    error_logger:error_report("mmmm local food"),
                    {BX,BY};
                {BX,BY,nothing_found} when length(MemoryList) =:= 0 -> 
                    % No local food, doesn't remember any food...
                    % Wander around until you starve poor human!
                    error_logger:error_report("I don't remember any food and I can see no food"),
                    {BX,BY};
                {_BX,_BY,nothing_found} ->
                    % No local food, does remember food however...
                    % Pathfind to some food you remember
                    NewPath = pathfind_to_item(MemoryList, {X,Y}, Olist),
                    error_logger:error_report("I gone done pathfound"),
                    [{PathX,PathY}|_Rest] = NewPath,
                    {PathX,PathY}
            end;
        very_hungry ->
            % need to search for food, boids a little, but also limit speed
            MemoryList = maps:keys(NewMemoryMap),
            % error_logger:error_report(MemoryList),
            case make_choice(Hlist,Zlist, NearestItem, NewHungerState, State) of
                {BX,BY} ->  
                    % Local food! Go forth hungry human!
                    % error_logger:error_report("mmmm local food"),
                    {BX,BY};
                {BX,BY,nothing_found} when length(MemoryList) =:= 0 -> 
                    % No local food, doesn't remember any food...
                    % Wander around until you starve poor human!
                    % error_logger:error_report("I don't remember and I can see no food"),
                    {BX,BY};
                {_BX,_BY,nothing_found} ->
                    % No local food, does remember food however...
                    % Pathfind to some food you remember
                    NewPath = pathfind_to_item(MemoryList, {X,Y}, Olist),
                    error_logger:error_report("I gone done pathfound"),
                    [{PathX,PathY}|_Rest] = NewPath,
                    {PathX,PathY}
            end;
        hungry ->
            % search for food, but also boids
            make_choice(Hlist,Zlist, NearestItem, NewHungerState, State);
        not_hungry ->
            % save any food you find to a map, boids as normal
            make_choice(Hlist,Zlist, NearestItem, NewHungerState, State)
    end,


    New_X_Velocity = X_Velocity + BoidsX,
    New_Y_Velocity = Y_Velocity + BoidsY,

    {Limited_X_Velocity,Limited_Y_Velocity} = case NewHungerState of
        tired ->
            % need to limit speed drastically
            boids_functions:limit_speed(?TIRED_LIMIT,X,Y,New_X_Velocity,New_Y_Velocity);
        very_hungry ->
            % need to search for food, boids a little, but also limit speed
            boids_functions:limit_speed(?HUNGRY_LIMIT,X,Y,New_X_Velocity,New_Y_Velocity);
        _ ->
            % need to search for food, boids a little, but also limit speed
            boids_functions:limit_speed(?LIMIT,X,Y,New_X_Velocity,New_Y_Velocity)
    end,

    TargetX = round(X + Limited_X_Velocity),
    TargetY = round(Y + Limited_Y_Velocity),  
    {NewX,NewY} = obstructed(Olist,X,Y,TargetX,TargetY,Limited_X_Velocity,Limited_Y_Velocity),
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
            {next_state,run,State#state{x=ReturnedX,y=ReturnedY,
                                        bearing = Bearing, tile = NewTile, 
                                        z_list = Zlist_Json, h_list = Hlist_Json,
                                        x_velocity = Limited_X_Velocity, 
                                        y_velocity = Limited_Y_Velocity,
                                        hunger = NewHunger, energy = NewEnergy,
                                        hunger_state = NewHungerState,
                                        memory_map = NewMemoryMap}}
    end.


%%%%%%==========================================================================
%%%%%% Event and Sync Functions
%%%%%%==========================================================================

pause(move, State) -> 
    %% If we get a move event start the timer again but don't actually move
    %% Ensure we will move after unpause.
    gen_fsm:send_event_after(State#state.timeout, move),
    {next_state, pause, State};
pause(unpause, #state{paused_state = PausedState} = State) ->
    {next_state,PausedState,State}.   

calc_state(_Current_state) ->
    run.
    
calc_runbearing(Rand,X,Y) ->
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
    {next_state,pause,StateData#state{paused_state = StateName}};

handle_event(zombify, StateName, #state{speed = Speed, x = X, y = Y, tile_size = TileSize,
                    num_columns = NumColumns, num_rows = NumRows,
                    tile = Tile, type = Type,
                    x_velocity = X_Velocity, y_velocity = Y_Velocity,
                    viewer = Viewer} = StateData) ->
    {ok,Zombie}=supervisor:start_child(zombie_sup,[X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,300,0]),
    zombie_fsm:start(Zombie),
    supervisor:terminate_child(human_sup, self()).
                           
handle_sync_event(get_state, _From, StateName, StateData) ->
    PropList = record_to_proplist(StateData),
    % take the pid out of the report for JSX
    PropListNoViewer = proplists:delete(viewer,PropList),
    % take the MemoryMap out of the report for JSX
    PropListNoMemory = proplists:delete(memory_map,PropListNoViewer),
    {reply, {ok,PropListNoMemory}, StateName,StateData}.

record_to_proplist(#state{} = Record) ->
    lists:zip(record_info(fields, state), tl(tuple_to_list(Record))).
 

%%%%%%==========================================================================
%%%%%% Boids Functions
%%%%%%==========================================================================
% Make choice is called with ->
%   (HumanList, ZombieList, NearestItem, HungerState, State)
make_choice([],[],_NearestItem, _HungerState, _State) ->
    {0,0};

%===========================Collision Avoidance=================================%
make_choice([{Dist, {_,{_,{{HeadX,HeadY},{_,_}}}}}|_],_,_,_,State) when Dist < ?PERSONAL_SPACE ->
    boids_functions:collision_avoidance(State#state.x, State#state.y, HeadX, HeadY,?COHESION_EFFECT);

%=============================Super Repulsor====================================%
make_choice(_,[{_Dist, {_,{_,{{HeadX,HeadY},{_,_}}}}}|_],_NearestItem, _, State) ->
    boids_functions:super_repulsor(State#state.x,State#state.y,HeadX,HeadY,?SUPER_EFFECT);

%===============================Flocking========================================%
make_choice(Hlist,_,_NearestItem, not_hungry, State) ->
    {Fx,Fy} = boids_functions:flocking(Hlist,State#state.x,State#state.y,?FLOCKING_EFFECT),
    {Vx,Vy} = boids_functions:velocity(Hlist,State#state.x_velocity,State#state.y_velocity,?VELOCITY_EFFECT),
    {(Fx+Vx),(Fy+Vy)};

make_choice(Hlist,_,_NearestItem, hungry, State) ->
    {Fx,Fy} = boids_functions:flocking(Hlist,State#state.x,State#state.y,?FLOCKING_EFFECT),
    {Vx,Vy} = boids_functions:velocity(Hlist,State#state.x_velocity,State#state.y_velocity,?VELOCITY_EFFECT),
    {(Fx+Vx),(Fy+Vy)};

%============================Hungry - Local Item================================%
% There is no zombie, time to eat!
make_choice(_,[],{_,{ItemX,ItemY,_,_}}, very_hungry, State) ->
    boids_functions:super_attractor(State#state.x,State#state.y,ItemX,ItemY,?SUPER_EFFECT);

make_choice(_,[],{_,{ItemX,ItemY,_,_}}, tired, State) ->
    boids_functions:super_attractor(State#state.x,State#state.y,ItemX,ItemY,?SUPER_EFFECT);

% Got some humans, but no food
make_choice(Hlist,_,nothing_found, very_hungry, State) ->
    {Fx,Fy} = boids_functions:flocking(Hlist,State#state.x,State#state.y,?FLOCKING_EFFECT),
    {Vx,Vy} = boids_functions:velocity(Hlist,State#state.x_velocity,State#state.y_velocity,?VELOCITY_EFFECT),
    {(Fx+Vx),(Fy+Vy),nothing_found};

make_choice(Hlist,_,nothing_found, tired, State) ->
    {Fx,Fy} = boids_functions:flocking(Hlist,State#state.x,State#state.y,?FLOCKING_EFFECT),
    {Vx,Vy} = boids_functions:velocity(Hlist,State#state.x_velocity,State#state.y_velocity,?VELOCITY_EFFECT),
    {(Fx+Vx),(Fy+Vy),nothing_found}.


%%%%%%==========================================================================
%%%%%% Functions for Boids Functions
%%%%%%==========================================================================

% A function to find the closest item to the human
get_nearest_item([],_) ->
    nothing_found;
get_nearest_item([I|Is], {HumanX, HumanY}) ->
    get_nearest_item(Is, {HumanX, HumanY}, I).

% Find the nearest item in sight
get_nearest_item([], _, NearestItem) ->
    NearestItem;
get_nearest_item([{ID,{X,Y,Type,Item}}|Is],{HumanX, HumanY}, {NID,{NearestX,NearestY,NType,NItem}}) ->
    BestItem = pythagoras:pyth(NearestX, NearestY, HumanX, HumanY),
    case pythagoras:pyth(X, Y, HumanX, HumanY) of
        Value when Value < BestItem ->
            get_nearest_item(Is, {HumanX, HumanY}, {ID, {X,Y,Type,Item}});
        _ -> 
            get_nearest_item(Is, {HumanX, HumanY}, {NID,{NearestX,NearestY,NType,NItem}})
    end.

% Ask astar2 for a path to an item, avoiding obstacles
pathfind_to_item([Head|Rest], CurrentPos, ObsList) ->
    NearestMemoryItem = nearest_memory_item(Rest, Head, CurrentPos),
    astar2:astar(NearestMemoryItem,CurrentPos, ObsList).

% Find the nearest item from memory
nearest_memory_item([], Nearest, _CurrentPos) ->
    Nearest;
nearest_memory_item([Head|Rest], Nearest, CurrentPos) ->
    NearestDist = astar2:dist_between(Nearest,CurrentPos),
    case astar2:dist_between(Head,CurrentPos) of
        Value when Value < NearestDist ->
            % Head of the list is closer than current best
            nearest_memory_item(Rest,Head,CurrentPos);
        _ ->
            % Current best is still best
            nearest_memory_item(Rest,Nearest,CurrentPos)
    end.  

%obstructed([],_X,_Y,NewX,NewY,_Velx,_VelY) ->
%    {NewX,NewY};
%obstructed(_Olist,X,Y,NewX,NewY,_VelX,_VelY) when X =:= NewX, Y =:= NewY ->
%    {NewX,NewY};
%obstructed([{_D,{_,{_,{{OX,OY},{_,_}}}}}|OlistTail],X,Y,NewX,NewY,VelX,VelY) when OX =:= NewX, OY =:= NewY->
%    error_logger:error_report("gflkf"),
%    {X,Y};
%obstructed([{_D,{_,{_,{{OX,OY},{_,_}}}}}|OlistTail],X,Y,NewX,NewY,VelX,VelY) ->
%    obstructed(OlistTail,X,Y,NewX,NewY,VelX,VelY).
obstructed([],_X,_Y,NewX,NewY,_Velx,_VelY) ->
    {NewX,NewY};
obstructed(Olist,X,Y,NewX,NewY,_VelX,_VelY) ->
    Member = lists:any(fun({A,B}) -> NewY div 5 == B andalso NewX div 5  == A end,Olist),
    case Member of
        true->
            {X,Y};
        false->
            {NewX,NewY}
    end.

%%%%%%==========================================================================
%%%%%% List Organisation and Setup Functions
%%%%%%==========================================================================

% Turn a list into something JSON can deal with.
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

% Build a list of local zombie entities that are in sight
build_zombie_list(Viewer, X, Y) ->
    ZombieList = viewer:get_zombies(Viewer),

    Z_DistanceList = lists:map(fun(
                                {ZomPid,{ZType,{{ZX,ZY},{ZX_Velocity,ZY_Velocity}}}}) ->
                                    {pythagoras:pyth(X,Y,ZX,ZY),
                                    {ZomPid,{ZType,{{ZX,ZY},
                                    {ZX_Velocity,ZY_Velocity}}}}} 
                                end,ZombieList),

    Z_FilteredList = lists:filter(
                                fun({Dist,{_,{_,{{_,_},{_,_}}}}}) ->
                                    Dist =< ?SIGHT
                                end,Z_DistanceList),

    Zlist = lists:keysort(1,Z_FilteredList),
    %return
    Zlist.

% Build a list of local zombie entities that are in sight
build_human_list(Viewer, X, Y) ->
    HumanList = viewer:get_humans(Viewer),
    
    NoSelfList = lists:keydelete(self(),1,HumanList),

    H_DistanceList = lists:map(fun(
                                {Hpid,{human,{{HX,HY},{HXV,HYV}}}}) -> 
                                    {pythagoras:pyth(X,Y,HX,HY),
                                    {Hpid,{human,{{HX,HY},
                                    {HXV,HYV}}}}} 
                            end,NoSelfList),

    H_FilteredList = lists:filter(
                                fun({Dist,{_,{_,{{_,_},{_,_}}}}}) ->
                                    Dist =< ?SIGHT
                                end,H_DistanceList),

    Hlist = lists:keysort(1,H_FilteredList),
    %return
    Hlist.

build_obs_list(Olist, X,Y) ->
    O_DistanceList = lists:map(fun({ObX,ObY}) -> {pythagoras:pyth(X,Y,ObX,ObY),{noPid,{obstruction,{{ObX,ObY},{0,0}}}}} end, Olist),
    %sort the list by distance
    lists:keysort(1,O_DistanceList).

% Build memory map for items
build_memory([], Map) ->
    Map;
build_memory([{Pid,{X,Y,Type,Name}}|Rest], Map) ->
    NewMap = maps:put({X,Y}, {Pid,Type,Name}, Map),
    build_memory(Rest, NewMap).
