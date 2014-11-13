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
        reply_entity/3,
        start_link/3]).

-define(SERVER, ?MODULE).

-type   coord() ::  pos_integer().

%%%% entityDict - a dictonary of entities within the tile
%%%% x and y origin - the origin of the tile
%%%% x and y limit - the edge of the tile
%%%% coords - a tuple containing {Xo,Yo, Xl,Yl}
-record(tile_state, {entityDict=dict:new(),
                    xorigin  ::  coord(),
                    yorigin  ::  coord(),
                    xlimit  ::  coord(),
                    ylimit  ::  coord(),
                    coords  ::  tuple(),
                    viewer  ::  pid(),
                    neighbours  ::  []}).  

%%%%%%=============================================================================
%%%%%% API
%%%%%%=============================================================================

%%%%%% Calls
get_population(Pid) ->
    gen_server:call(Pid, get_population).

get_geometry(Pid) ->
    gen_server:call(Pid, get_geometry).

get_viewer(Pid) ->
    gen_server:call(Pid, get_viewer).

get_neighbours(Pid) ->
    gen_server:call(Pid, get_neighbours).

reply_entity(Pid, State, Entity) ->
    gen_server:call(Pid, {reply_entity, State, Entity}).

%%%%%% Casts
%make this also call a reply
summon_entity(Pid, Entity) ->
    gen_server:cast(Pid, {summon_entity, Entity}).

remove_entity(Pid, Entity) ->
    gen_server:cast(Pid, {remove_entity, Entity}).

update_entity(Pid, Entity, Pos, Heading, _Speed) ->
    gen_server:cast(Pid, {update_entity, Entity, Pos}).

set_geometry(Pid,Xorigin,Yorigin,Size) ->
    gen_server:cast(Pid, {set_geometry, Xorigin, Yorigin, Size}).

set_viewer(Pid, ViewerPid) ->
    gen_server:cast(Pid, {set_viewer, ViewerPid}).

set_neighbours(Pid, NeighbourPids) ->
    gen_server:cast(Pid, {set_neighbours, NeighbourPids}).


%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Start the server.
%%%% @end
%%%%------------------------------------------------------------------------------
start_link(X,Y,Size) -> %I've had to change this??? I don't know whats going on here
    gen_server:start_link(?MODULE, [X,Y,Size], []).

%%%%%%=============================================================================
%%%%%% gen_server Callbacks
%%%%%%=============================================================================


% look on learn you an erlang for dealing with S
%init([]) ->
%    {ok, #tile_state{}};
%init([S]) ->
    %io:format(S),
    %{ok, #tile_state{}};
init([X,Y,Size]) ->
    set_geometry(self(),X,Y,Size),
    {ok, #tile_state{}}.

%%%%%% Calls

handle_call(get_population,_From,State) ->
    {reply,State#tile_state.entityDict,State};
handle_call(get_geometry,_From,State) ->
    {reply,State#tile_state.coords, State};
handle_call(get_viewer,_From,State) ->
    {reply,State#tile_state.viewer};
handle_call(get_neighbours,_From,State) ->
    {reply,State#tile_state.neighbours};
handle_call({reply_entity, State, Entity} ,_From, State) ->
    %{ID,{Pos}} = Entity,
    {reply,State#tile_state.entityDict, State}.

%%%%%% Casts

%%%% Handle summon entity, ensure that no entities end up on the same coordinate
% needs to call reply_entity
handle_cast({summon_entity, Entity}, State) when size(State#tile_state.entityDict) =/= 0 ->
    {ID,{X,Y}} = Entity,
    {noreply,State#tile_state{entityDict = add_unique(ID,{X,Y},State#tile_state.entityDict)}};
handle_cast({summon_entity, Entity}, State) when size(State#tile_state.entityDict) =:= 0 ->
    {ID,{X,Y}} = Entity,
    {noreply,State#tile_state{entityDict = dict:store(ID,{X,Y},State#tile_state.entityDict)}};
%%%% Handle delete entity calls
handle_cast({remove_entity, Entity}, State) ->
    {ID,{_,_}} = Entity,
    {noreply,State#tile_state{entityDict = dict:erase(ID,State#tile_state.entityDict)}};
%%%% Handle update entity calls
handle_cast({update_entity, Entity, Pos, Heading, _Speed}, State) ->
    {ID,{_,_}} = Entity,
    case dict:is_key(ID,State#tile_state.entityDict) of
        true ->
            case Heading of
                n ->    {noreply,State#tile_state{entityDict = dict:store(ID,{Pos},State#tile_state.entityDict)}};
                e ->    {noreply,State#tile_state{entityDict = dict:store(ID,{Pos},State#tile_state.entityDict)}};
                s ->    {noreply,State#tile_state{entityDict = dict:store(ID,{Pos},State#tile_state.entityDict)}};
                w ->    {noreply,State#tile_state{entityDict = dict:store(ID,{Pos},State#tile_state.entityDict)}}
            end;
        false ->
            {noreply,State#tile_state{entityDict = summon_entity(State,Entity)}}
    end;  
%%%% Handle set geometry calls
handle_cast({set_geometry, X, Y, Size}, State) ->
    {noreply,State#tile_state{xorigin = X, yorigin = Y, xlimit = X+Size, ylimit = Y+Size, coords = {X,Y,X+Size,Y+Size}}};
%%%% Handles setting of tiles viewer
handle_cast({set_viewer, ViewerPid}, State) ->
    {noreply,State#tile_state{viewer = ViewerPid}};
%%%% Add nearby tiles viewers
handle_cast({set_neighbours, NeighbourPids}, State) ->
    {noreply,State#tile_state{neighbours = State#tile_state.neighbours++NeighbourPids}, update_viewers({State#tile_state{}})};
%%%% Handle the cast to update the viewers
handle_cast({update_viewers}, State) ->
    {noreply,update_viewers(State#tile_state{}, State#tile_state.neighbours)}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================

%%%%%% Calls
%get_population(Pid) ->
    %gen_server:call(Pid, get_population).

%get_geometry(Pid) ->
    %gen_server:call(Pid, get_geometry).

%get_viewer(Pid) ->
    %gen_server:call(Pid, get_viewer).

%get_neighbours(Pid) ->
    %gen_server:call(Pid, get_neighbours).

%%%%%%% Casts
%summon_entity(Pid, Entity) ->
    %gen_server:cast(Pid, {summon_entity, Entity}).

%remove_entity(Pid, Entity) ->
    %gen_server:cast(Pid, {remove_entity, Entity}).

%update_entity(Pid, Entity, Pos, Heading, _Speed) ->
    %gen_server:cast(Pid, {update_entity, Entity, Pos}).

%set_geometry(Pid,Xorigin,Yorigin,Size) ->
    %gen_server:cast(Pid, {set_geometry, Xorigin, Yorigin, Size}).

%set_viewer(Pid, ViewerPid) ->
    %gen_server:cast(Pid, {set_viewer, ViewerPid}).

%set_neighbours(Pid, NeighbourPids) ->
    %gen_server:cast(Pid, {set_neighbours, NeighbourPids}).


%% A function to periodically update the viewers
%% Currently this is carried out every 5 seconds, this is just an arbitratry value
%% until a more permanent solution is decided upon
update_viewers(Pid) ->
    receive
    after
        5000 ->
        gen_server:cast(Pid, {update_viewers})
    end.

%%%%%% Functions

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

% Still need to figure out how to update a zombies viewer


% get pid of registered process wheris(module)

% observer:start().

% sys:get_state(Pid).

