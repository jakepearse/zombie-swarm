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

%%%% entityDict - a dictonary of entities within the tile
%%%% x and y origin - the origin of the tile
%%%% x and y limit - the edge of the tile
%%%% coords - a tuple containing {Xo,Yo, Xl,Yl}
%%%% viewer - the assigned viewer of the tile
%%%% neihbours - a list of the neighbouring tiles viewers

-record(tile_state, {entityDict=dict:new(), % :: dict:new_dict()
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
    {ok, #tile_state{}}.

%%%%-Calls----------------------------------------------------------------------

handle_call(get_population,_From,State) ->
    {reply, make_usable(dict:to_list(State#tile_state.entityDict),[]),State};

handle_call(get_geometry,_From,State) ->
    {reply,State#tile_state.coords, State};

handle_call(get_viewer,_From,State) ->
    {reply,State#tile_state.viewer};

handle_call(get_neighbours,_From,State) ->
    {reply,State#tile_state.neighbours};

handle_call(get_state,_From,State) ->
    {reply,State,State};

handle_call({update_entity, Entity, Pos, _Bearing, _Speed},_From, State) ->
    {ID,{_,_}} = Entity,
    NewDict = dict:store(ID,Pos,State#tile_state.entityDict),
    {reply,Pos,State#tile_state{entityDict = NewDict}}.


%%%%-Casts----------------------------------------------------------------------

%%%% Handle summon entity, ensure that no entities end up on the same coordinate
handle_cast({summon_entity, Entity}, State) when 
        size(State#tile_state.entityDict) =/= 0 ->
    {ID,{X,Y}} = Entity,
    {noreply,State#tile_state{entityDict = 
        add_unique(ID,{X,Y},State#tile_state.entityDict)}};
handle_cast({summon_entity, Entity}, State) when 
        size(State#tile_state.entityDict) =:= 0 ->
    {ID,{X,Y}} = Entity,
    {noreply,State#tile_state{entityDict = 
        dict:store(ID,{X,Y},State#tile_state.entityDict)}};

%%%% Handle delete entity calls
handle_cast({remove_entity, Entity}, State) ->
    {ID,{_,_}} = Entity,
    {noreply,State#tile_state{entityDict = 
        dict:erase(ID,State#tile_state.entityDict)}};

%%%%%%%%%%%%%% Old, probably good to delete
% %%%% Updates an entities position on the tile 
% handle_cast({update_entity, Entity, Pos, _Bearing, _Speed}, State) ->
%     {ID,{_,_}} = Entity,
%     case dict:is_key(ID,State#tile_state.entityDict) of
%         true ->
%             {noreply,State#tile_state{entityDict = 
%                 dict:store(ID,Pos,State#tile_state.entityDict)}};
%         false ->
%             {noreply,State#tile_state{entityDict = 
%                 summon_entity(State,{ID,Pos})}}
%     end; 

%%%% Handle set geometry calls
handle_cast({set_geometry, X, Y, Size}, State) ->
    {noreply,State#tile_state{xorigin = X, yorigin = Y, xlimit = X+Size-1, 
        ylimit = Y+Size-1, coords = {X,Y,X+Size-1,Y+Size-1,Size}}};

%%%% Handles setting of tiles viewer
handle_cast({set_viewer, ViewerPid}, State) ->
    {noreply,State#tile_state{viewer = ViewerPid}};

%%%% Add nearby tiles viewers
handle_cast({set_neighbours, NeighbourPids}, State) ->
    {noreply,State#tile_state{neighbours = NeighbourPids}};

%%%% Handle the cast to update the viewers
handle_cast({update_viewers}, State) ->
    {noreply,update_viewers(State#tile_state{}, State#tile_state.neighbours)};

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

%% A function to periodically update the viewers
%% Currently this is carried out every 5 seconds, 
%% this is just an arbitratry value
%% until a more permanent solution is decided upon
update_viewers(Pid) ->
    receive
    after
        5000 ->
        gen_server:cast(Pid, {update_viewers})
    end.

%% Ensure new entity is in an untaken position
%% a bit ugly, but it works
add_unique(ID, Pos, Dict) ->
    {X,Y} = Pos,
    List = dict:to_list(Dict),
    case check_dict(List, ID, Pos) of
        false ->
            dict:store(ID, {X,Y}, dict:from_list(List));
        true ->
            add_unique(ID, {X+1,Y+1}, Dict)
    end.

%% Check dictinary for current postion
-spec check_dict(list(),_,_ ) ->  boolean().
check_dict([],_,_) -> false;
check_dict([X|Xs],ID,{X2,Y2}) ->
    {_,{X1,Y1}} = X,
    if (X1=/=X2) or (Y1=/=Y2) ->
        check_dict(Xs, ID, {X2,Y2});
        true -> true
    end.

%% This function iterates through the list of nearby viewers
%% For each of these, it updates them with the current dictionary of entities
update_viewers(State, []) -> update_viewers(State);
update_viewers(State, [X|Xs]) ->
    viewer:update(X,{self(),State#tile_state.entityDict}),
    update_viewers(State, Xs).

% Turn the dictionary into something usable by the client
-spec make_usable(list(),list()) -> list().
make_usable(A,B) ->
    make_usable(A,B,0).
make_usable([],[],_) -> [];
make_usable([],A,_) -> A;
make_usable([L|Ls],A,Num) ->
    {_Id,{X,Y}} = L,
    B = [[Num,X,Y]] ++ A,
    make_usable(Ls,B,Num+1).

%%%%-Notes----------------------------------------------------------------------

% Still need to figure out how to update a zombies viewer

% get pid of registered process wheris(module)

% observer:start().

% sys:get_state(Pid).

% eventually, entityDict needs to be a list of lists
%   when this is done, replace z1,z2,z3 etc etc with the Pid of the entities
%       entities in the list will be in the format [[id,x,y],[id,x,y]]
%           id = "pid", x = int, y = int

% update_entity needs to be a call

% tiles within tiles?
%   quadtree like datastructure

% list returning in strange order
% need to fix this
% pid_to_list for pid
% list_to_pid for reverse
% need to make  a pid to string function

% ctrl + g > to line
