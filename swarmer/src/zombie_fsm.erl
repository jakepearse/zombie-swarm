-module(zombie_fsm).
-author("Robert Hales rsjh3@kent.ac.uk").
-define(AIMLESS_STAY_COURSE, 8).

-include_lib("include/swarmer.hrl").
-behaviour(gen_fsm).

%gen_fsm implementation
-export([code_change/4,handle_event/3,handle_sync_event/4,
		 handle_info/3,init/1,terminate/3]).

-export([start_link/9,aimless/2,initial/2,aimless_search/2,active/2,
         active_search/2,chasing/2,chasing_search/2,calc_state/1,
         calc_aimlessbearing/3,start/1,pause/2,get_surrounding_humans/1,get_surrounding_zombies/1,
		 find_visible/2,find_visible/3,startzombie/1, get_all_state/1]).

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
                type,
                paused_state,
				fitness,
				bestfitness,
				bestx,
				besty,
				xvelocity = 0,
				yvelocity = 0
				}).

start_link(X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,Bearing) -> 
	gen_fsm:start_link(?MODULE,[X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,Bearing],[]).


%%% API!.

start(_Pid) ->
    ok.

startzombie(Pid) ->
    gen_fsm:send_event(Pid,startzombie).

pause(Pid) ->
    gen_fsm:send_all_state_event(Pid, pause).

unpause(Pid) -> 
    gen_fsm:send_event(Pid,unpause).

get_state(Pid) ->
    catch gen_fsm:sync_send_all_state_event(Pid, get_state).

get_all_state(Pid) ->
    catch gen_fsm:sync_send_all_state_event(Pid, get_all_state).
    

init([X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,_Bearing]) ->
	random:seed(erlang:now()),
    tile:summon_entity(Tile,{self(),{X,Y}, zombie}),
	{ok,initial,#state{tile = Tile,viewer = Viewer, x = X, y = Y,speed=Speed, 
                       bearing=random:uniform(360),type =zombie,
                       tile_size = TileSize, num_columns = NumColumns, 
                       num_rows = NumRows,bestx = X,besty = Y}}.

%%%%%%==========================================================================
%%%%%% State Machine
%%%%%%==========================================================================

initial(startzombie,State) ->
    gen_fsm:send_event_after(State#state.speed, move),
	{next_state,aimless_search,State}.
	
aimless(move,#state{speed = Speed, x = X, y = Y, tile_size = TileSize,
                    num_columns = NumColumns, num_rows = NumRows,
                    tile = Tile, type = Type, viewer = Viewer, fitness=Fitness} = State) ->
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
                    NewViewer = Viewer,
                    Tile;
                {_, NewXTile, _, NewYTile} ->
                    tile:remove_entity(Tile, self(), Type),
                    T = list_to_atom("tile" ++  "X" ++ integer_to_list(NewXTile) ++  "Y" ++ integer_to_list(NewYTile)),
                    NewViewer = tile:get_viewer(T),
                    T
            end,

            {ReturnedX,ReturnedY} = tile:update_entity(NewTile,{self(),{X,Y},Type},{NewX, NewY},Bearing,Speed,Fitness),
            gen_fsm:send_event_after(State#state.speed, move),
            {next_state,aimless_search,State#state{x=ReturnedX,y=ReturnedY,bearing = Bearing, tile = NewTile, viewer = NewViewer}}
    end.


aimless_search(move,#state{x = X, y = Y, bestfitness = BestFitness, tile_size = TileSize,
                    num_columns = NumColumns, num_rows = NumRows,bearing = Bearing, speed = Speed,
                    tile = Tile, type = Type, viewer = Viewer, bestx= BestX, besty = BestY, fitness = Fitness} =  State) ->
	Humans = get_surrounding_humans(State#state.viewer),
	%error_logger:error_report(Humans),
	case pso:zombie_target(X,Y,Humans) of
        notarget ->
            % gen_fsm:send_event_after(State#state.speed, move),
            gen_fsm:send_event(self(),move),
            {next_state,aimless,State};
        
        {Distance,{_HumanPid,{Hx,Hy}}} -> 
			% first check if this is a better position that any previous one
        	{NewBestFitness,NewBestX,NewBestY} = case Distance < BestFitness of
        		true ->
                    % best state eva
						{Distance,X,Y};
        		false ->
				% there's nothing to do here
					{BestFitness,BestX,BestY}
        	end,
        
        %this is where I call the pso to give me my new velocity
        Zombies = get_surrounding_zombies(State#state.viewer),
        %error_logger:error_report(Zombies),
        {Vx,Vy} = pso:velocity(3,0.8,X,Y,State#state.xvelocity,State#state.yvelocity,State#state.bestx,State#state.besty,Hx,Hy,Zombies),
        NewX = X+Vx,
        NewY = Y+Vy,
        
        
        {NewTile,NewViewer} = case (NewX < 0) or (NewY < 0) or (NewX > NumColumns * (TileSize-1)) or (NewY > NumRows * (TileSize-1)) of
			true -> % We are off the screen!
				{stop, shutdown, State},
				{offGrid,offGrid};
            false ->
                % This calculates if the human is still in it's initial tile
					case {trunc(X) div TileSize, trunc(NewX) div TileSize, trunc(Y) div TileSize, trunc(NewY) div TileSize} of
                        {XTile, XTile, YTile, YTile} -> % In same tile
							{Tile,Viewer};
						{_, NewXTile, _, NewYTile} ->
							tile:remove_entity(Tile, self(), Type),
							T = list_to_atom("tile" ++  "X" ++ integer_to_list(NewXTile) ++  "Y" ++ integer_to_list(NewYTile)),
							V = tile:get_viewer(T),
							{T,V}
                    end
        end,
		
		{ReturnedX,ReturnedY} = tile:update_entity(NewTile,{self(),{X,Y},Type},{NewX, NewY},Bearing,Speed,Fitness),
		
		%It's possible for the bestfitness to fall to <1 (the zombie ctaches the human) and then it will likely never chase a new target
		% thats why the all head southeast
		NewNewBestFitness = case BestFitness < 0.1 of
			true ->
				infinity;
			false ->
				NewBestFitness
		end,
		% I feel dumber just from writing that
		
        gen_fsm:send_event_after(State#state.speed, move),
        	{next_state,aimless_search,
        	State#state{
				tile = NewTile,
				viewer = NewViewer,
				x = ReturnedX,
				y = ReturnedY,
				fitness = Distance,
				bestfitness = NewNewBestFitness,
				bestx = NewBestX,
				besty = NewBestY,
				xvelocity = ReturnedX - X,
				yvelocity = ReturnedY - Y
				}
        	}
    end.
    
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
get_surrounding_humans(Viewer) ->
	viewer:get_humans(Viewer).
get_surrounding_zombies(Viewer) ->
	viewer:get_zombies(Viewer).
		
calc_state(_State) ->
	aimless.

find_visible(All,State) ->
	Visible = [],
	find_visible(All,State,Visible).
find_visible([],_State,Visible) ->
	Visible;
find_visible([[{Pid,{_Otherx,_Othery}}]|_Tail],_State,_Visible) ->
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

handle_sync_event(get_state, _From, StateName, 
                  #state{x = X, y = Y, speed = Speed, type = Type,
                         bearing = Bearing} = StateData) ->
    {reply,{ok,#entity_status{id = self(), x = X, y = Y, type = Type, 
                              current_activity = StateName, speed = Speed,
                              bearing = Bearing}},StateName,StateData};

handle_sync_event(get_all_state, _From, StateName, State) ->
    {reply,{ok,State},StateName,State}.
