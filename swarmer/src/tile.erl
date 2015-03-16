-module(tile).
-author("Joe Mitchard jm710@kent.ac.uk").

-behaviour(gen_server).

%%%% API
-export([start_link/4]).

%%%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%% tile functions
-export([summon_entity/2,
        remove_entity/3,
        update_entity/6,
        get_geometry/1,
        set_viewer/2,
        get_viewer/1,
        set_neighbours/2,
        get_neighbours/1,
        terminate/1,
        get_state/1,
        place_item/2,
        remove_item/2,
        check_obs/2,
        set_obs_list/2]).


-define(SERVER, ?MODULE).

-type   coord() ::  pos_integer().
-type   pos()  ::  {pos_integer(),pos_integer()}.
-type   entity()  ::  {pid(),{pos_integer(),pos_integer()}}.

%%%% entity_map - a dictonary of entities within the tile
%%%% x and y origin - the origin of the tile
%%%% x and y limit - the edge of the tile
%%%% coords - a tuple containing {Xo,Yo, Xl,Yl}
%%%% viewer - the assigned viewer of the tile
%%%% neihbours - a list of the neighbouring tiles viewers

-record(state, {zombie_map=maps:new() :: maps:maps(),
                human_map=maps:new() :: maps:maps(),
                item_map=maps:new() :: maps:maps(),
                xorigin  ::  coord(),
                yorigin  ::  coord(),
                xlimit  ::  coord(),
                ylimit  ::  coord(),
                coords  ::  tuple(),
                viewer  ::  pid(),
                neighbours  ::  [pid()],
                obs_list=[] :: list()}). 

%%%%%%==========================================================================
%%%%%% API
%%%%%%==========================================================================

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Start the server.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec start_link(atom(), coord(),coord(),pos_integer()) -> ok.
start_link(Name,X,Y,Size) ->
    gen_server:start_link(?MODULE, [Name,X,Y,Size], []).

%%%%-Calls----------------------------------------------------------------------
%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Return the geometry of the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec get_geometry(pid()) -> ok.
get_geometry(Pid) ->
    gen_server:call(Pid, get_geometry).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Return the assigned viewer of the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec get_viewer(pid()) -> ok.
get_viewer(Pid) ->
    gen_server:call(Pid, get_viewer).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Return a list of the associated viewers from the neighbouring tiles.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec get_neighbours(pid()) -> ok.
get_neighbours(Pid) ->
    gen_server:call(Pid, get_neighbours).


set_obs_list(Pid,New_obs_list) ->
	gen_server:call(Pid,{set_obs_list,New_obs_list}).

%%% provides api call to check if a pos() is obstructed
check_obs(Pid,Pos) ->
	gen_server:call(Pid,{check_obs,Pos}).
	
%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Update the entities position on the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec update_entity(pid(),entity(),pos(),_,_,_) -> ok.
update_entity(Pid, Entity, Pos, Bearing, _Speed, Velocity) ->
    gen_server:call(Pid, {update_entity, Entity, Pos, Bearing, _Speed,Velocity}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Place an item on the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
place_item(Pid, Item) ->
    gen_server:call(Pid, {place_item, Item}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Remove an item from the tile
%%%% @end
%%%%----------------------------------------------------------------------------
remove_item(Pid, Item) ->
    gen_server:call(Pid, {remove_item, Item}).

%%%%-Casts----------------------------------------------------------------------

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Add an entity to the tile.
%%%% Entity should be sent in the form of {Pid,{X,Y}}.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec summon_entity(pid(),entity()) -> ok.
summon_entity(Pid, Entity) ->
    gen_server:cast(Pid, {summon_entity, Entity}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Remove an entity from the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec remove_entity(pid(),entity(), atom()) -> ok.
remove_entity(Pid, Entity, Type) ->
    gen_server:cast(Pid, {remove_entity, Entity, Type}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Return the state of the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec get_state(pid()) -> ok.
get_state(Pid) ->
  gen_server:call(Pid,get_state).
 
  
%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Assign a viewer to the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec set_viewer(pid(),pid()) -> ok.
set_viewer(Pid, ViewerPid) ->
    gen_server:cast(Pid, {set_viewer, ViewerPid}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Assign the neighbouring tiles viewers.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec set_neighbours(pid(), list()) -> ok.
set_neighbours(Pid, NeighbourPids) ->
    gen_server:cast(Pid, {set_neighbours, NeighbourPids}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% End the tile process.
%%%% @end
%%%%----------------------------------------------------------------------------
terminate(Pid) ->
    gen_server:cast(Pid, terminate).
    

%%%%%%==========================================================================
%%%%%% gen_server Callbacks
%%%%%%==========================================================================

init([Name,X,Y,Size]) ->
    % this registers the name and pid of the process
    erlang:register(Name, self()),
    {ok, #state{xorigin = X, yorigin = Y, xlimit = X+Size-1, ylimit = Y+Size-1, 
                coords = {X,Y,X+Size-1,Y+Size-1,Size}}}.

%%%%-Calls----------------------------------------------------------------------
handle_call(get_geometry,_From,State) ->
    {reply,State#state.coords, State};

handle_call(get_viewer,_From,State) ->
    {reply,State#state.viewer,State};

handle_call(get_neighbours,_From,State) ->
    {reply,State#state.neighbours,State};

handle_call(get_state,_From,State) ->
    {reply,State,State};

handle_call({place_item, {ID,X,Y,Type,Item}}, _From, #state{item_map = ItemMap} = State) ->
    NewMap = maps:put(ID,{X,Y,Type,Item}, ItemMap),
    update_viewers(State#state.neighbours, items, NewMap),
    {reply, ok, State#state{item_map = NewMap}};

handle_call({remove_item, {ID,_,_,_,_}}, _From, #state{item_map = ItemMap} = State) ->
    NewMap = maps:remove(ID, ItemMap),
    update_viewers(State#state.neighbours, items, NewMap),
    {reply, ok, State#state{item_map = NewMap}};

%%%% Updates the entities position on the tile.
%%%% Will also deal with a new entitiy being moved onto the tile
handle_call({update_entity, {ID,{_,_},Type}, Pos, _Bearing, _Speed, Velocity},_From, State) when Type == zombie ->
    NewMap = maps:put(ID,{Type,{Pos,Velocity}},State#state.zombie_map),
    update_viewers(State#state.neighbours, Type, NewMap),
    {reply,Pos,State#state{zombie_map = NewMap}};
handle_call({update_entity, {ID,{_,_},Type}, Pos, _Bearing, _Speed, Velocity},_From, State) when Type == human ->
    NewMap = maps:put(ID,{Type,{Pos, Velocity}},State#state.human_map),
    update_viewers(State#state.neighbours, Type, NewMap),
    {reply,Pos,State#state{human_map = NewMap}};
    
%%%% pushes a list of obstructed coordinates into the state
handle_call({set_obs_list,New_obs_list},_From,State) ->
    update_viewers(State#state.neighbours, obs_list, New_obs_list),
	{reply,ok,State#state{obs_list=New_obs_list}};

%%% boolean check for obstruction of a pos()
handle_call({check_obs,Pos},_From,State) ->
	{reply,do_check_obs(Pos,State#state.obs_list),State}.

%%%%-Casts----------------------------------------------------------------------

%%%% Handle summon entity, ensure that no entities end up on the same coordinate
%%%% This is only used for the initialisation stage of the application.
%%%% No reply because the environment doesn't care where the new zombie ends up.
handle_cast({summon_entity,{ID,{X,Y},Type}}, #state{zombie_map =Zombie_Map} =State) when Type == zombie ->
    NewMap = maps:put(ID,{Type,{{X,Y},{0,0}}},Zombie_Map),
    update_viewers(State#state.neighbours, Type, NewMap),
    {noreply,State#state{zombie_map = NewMap}};
handle_cast({summon_entity,{ID,{X,Y}, Type}},#state{human_map =Human_Map} =State) when Type == human ->
    NewMap = maps:put(ID,{Type,{{X,Y},{0,0}}},Human_Map),
    update_viewers(State#state.neighbours, Type, NewMap),
    {noreply,State#state{human_map = Human_Map}};

%%%% Handle delete entity calls
handle_cast({remove_entity,ID, Type},#state{zombie_map =Zombie_Map} =State) when Type == zombie ->
    NewMap = maps:remove(ID,Zombie_Map),
    update_viewers(State#state.neighbours, Type, NewMap),
    {noreply,State#state{zombie_map = NewMap}};
handle_cast({remove_entity,ID, Type},#state{human_map =Human_Map} =State) when Type == human ->
    NewMap = maps:remove(ID,Human_Map),
    update_viewers(State#state.neighbours, Type, NewMap),
    {noreply,State#state{human_map = NewMap}};

%%%% Handles setting of tiles viewer
handle_cast({set_viewer, ViewerPid}, State) ->
    {noreply,State#state{viewer = ViewerPid}};

%%%% Add nearby tiles viewers
handle_cast({set_neighbours, NeighbourPids}, State) ->
    {noreply,State#state{neighbours = NeighbourPids}};

%%%% Handle cast to end the system normally
handle_cast(terminate, State) ->
    {stop,normal,State}.

%%%%-Gen-Server-API-------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%==========================================================================
%%%%%% Internal Functions
%%%%%%==========================================================================

%% a function to add a new entity to the entity_map
%% eventually will need to become more inteligent than just X+1,Y+1
% add_unique(ID, {X,Y}, Map) ->
%     Values = maps:values(Map),
%     case lists:member({X,Y}, Values) of
%         false ->
%             maps:put(ID, {X,Y}, Map);
%         true ->
%             add_unique(ID, {X+1,Y+1}, Map)
%     end.

update_viewers([], _Type, _EntityMap) ->
    [];
update_viewers([V|Vs], zombie, EntityMap) ->
    viewer:update_zombies(V, {self(), maps:to_list(EntityMap)}),
    update_viewers(Vs, zombie, EntityMap);
update_viewers([V|Vs], human, EntityMap) ->
    viewer:update_humans(V, {self(), maps:to_list(EntityMap)}),
    update_viewers(Vs, human, EntityMap);
update_viewers([V|Vs], items, ItemMap) ->
    viewer:update_items(V, {self(), maps:to_list(ItemMap)}),
    update_viewers(Vs, items, ItemMap);
update_viewers([V|Vs], obs_list, ObsList) ->
    viewer:update_obs(V, {self(), ObsList}),
    update_viewers(Vs, obs_list, ObsList).



%%%==============
%%% This is called to check if a coordinate pair is obstructed
%%% =============
-spec do_check_obs(pos(),list()) -> boolean().
do_check_obs({X,Y},Obs_list) ->
	lists:any(fun(C) -> C=={X div 5,Y div 5} end,Obs_list).
	
%%%%-Notes----------------------------------------------------------------------

% get pid of registered process wheris(module)

% observer:start().

% sys:get_state(Pid).
