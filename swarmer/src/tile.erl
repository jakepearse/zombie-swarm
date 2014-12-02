-module(tile).
-author("Joe Mitchard jm710@kent.ac.uk").

-behaviour(gen_server).

%%%% API
-export([start_link/3]).

%%%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%% tile functions
-export([get_population/1,
        summon_entity/2,
        remove_entity/2,
        update_entity/5,
        set_geometry/4,
        get_geometry/1,
        set_viewer/2,
        get_viewer/1,
        set_neighbours/2,
        get_neighbours/1,
        terminate/1,
        get_state/1]).


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

-record(state, {entity_map=maps:new() :: maps:maps(),
                    xorigin  ::  coord(),
                    yorigin  ::  coord(),
                    xlimit  ::  coord(),
                    ylimit  ::  coord(),
                    coords  ::  tuple(),
                    viewer  ::  pid(),
                    neighbours  ::  [pid()]}). 

%%%%%%==========================================================================
%%%%%% API
%%%%%%==========================================================================

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Start the server.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec start_link(coord(),coord(),pos_integer()) -> ok.
start_link(X,Y,Size) ->
    gen_server:start_link(?MODULE, [X,Y,Size], []).

%%%%-Calls----------------------------------------------------------------------

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Return the population of the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec get_population(pid()) -> ok.
get_population(Pid) ->
    gen_server:call(Pid, get_population).

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

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Update the entities position on the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec update_entity(pid(),entity(),pos(),_,_) -> ok.
update_entity(Pid, Entity, Pos, Bearing, _Speed) ->
    gen_server:call(Pid, {update_entity, Entity, Pos, Bearing, _Speed}).


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
-spec remove_entity(pid(),entity()) -> ok.
remove_entity(Pid, Entity) ->
    gen_server:cast(Pid, {remove_entity, Entity}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Set the boundaries of the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec set_geometry(pid(),coord(),coord(),pos_integer()) -> ok.
set_geometry(Pid,Xorigin,Yorigin,Size) ->
    gen_server:cast(Pid, {set_geometry, Xorigin, Yorigin, Size}).

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

init([X,Y,Size]) ->
    set_geometry(self(),X,Y,Size),
    {ok, #state{}}.

%%%%-Calls----------------------------------------------------------------------

handle_call(get_population, _From, State) ->
    Report = build_report(State#state.entity_map),
    {reply, Report, State};

handle_call(get_geometry,_From,State) ->
    {reply,State#state.coords, State};

handle_call(get_viewer,_From,State) ->
    {reply,State#state.viewer};

handle_call(get_neighbours,_From,State) ->
    {reply,State#state.neighbours};

handle_call(get_state,_From,State) ->
    {reply,State,State};

handle_call({update_entity, {ID,{_,_}}, Pos, _Bearing, _Speed},_From, State) ->
    NewMap = maps:put(ID,Pos,State#state.entity_map),
    update_viewers(State#state.neighbours, NewMap),
    {reply,Pos,State#state{entity_map = NewMap}}.

%%%%-Casts----------------------------------------------------------------------

%%%% Handle summon entity, ensure that no entities end up on the same coordinate
handle_cast({summon_entity,{ID,{X,Y}}},#state{entity_map =EntityMap} =State)->
    NewMap = add_unique(ID,{X,Y},EntityMap),
    update_viewers(State#state.neighbours, NewMap),
    {noreply,State#state{entity_map = NewMap}};

%%%% Handle delete entity calls
handle_cast({remove_entity,{ID,{_,_}}},#state{entity_map =EntityMap} =State)->
    {noreply,State#state{entity_map = 
      maps:remove(ID,EntityMap)}};

%%%% Handle set geometry calls
handle_cast({set_geometry, X, Y, Size}, State) ->
    {noreply,State#state{xorigin = X, yorigin = Y, xlimit = X+Size-1, 
        ylimit = Y+Size-1, coords = {X,Y,X+Size-1,Y+Size-1,Size}}};

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
add_unique(ID, {X,Y}, Map) ->
    Values = maps:values(Map),
    case lists:member({X,Y}, Values) of
        false ->
            maps:put(ID, {X,Y}, Map);
        true ->
            add_unique(ID, {X+1,Y+1}, Map)
    end.


build_report(EntityMap) ->
    DictList = maps:to_list(EntityMap),
    lists:map(
        fun({ID,{X,Y}}) ->
            [{id,list_to_binary(pid_to_list(ID))},{x,X},{y,Y}]
        end, DictList).

update_viewers([], _EntityMap) -> 
    [];
update_viewers([V|Vs], EntityMap) ->
    viewer:update_population(V, {self(), maps:to_list(EntityMap)}),
    update_viewers(Vs, EntityMap).

%%%%-Notes----------------------------------------------------------------------

% get pid of registered process wheris(module)

% observer:start().

% sys:get_state(Pid).

% update_entity needs to be a call

% tiles within tiles?
%   quadtree like datastructure

% ctrl + g > to line

% TODO
% Change what the get_population
%   needs to return [["pid", X, Y, type, heading, speed, current_state]]

% Need to work out when moving, if you remain in the time
% if not, tell the neigbour that it now owns the zombie and remove zombie
