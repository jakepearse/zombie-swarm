-module(tile).
-author("Joe Mitchard jm710@kent.ac.uk").

-behaviour(gen_server).

%%%% API
-export([start_link/0]).

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
        update_entity/3]).

-define(SERVER, ?MODULE).

%%%% zombieList : a list of all the Zombies on the current tile
-record(tile_state,{entityDict=dict:new()}).

%%%%%%=============================================================================
%%%%%% API
%%%%%%=============================================================================

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Start the server.
%%%% @end
%%%%------------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%%%%=============================================================================
%%%%%% gen_server Callbacks
%%%%%%=============================================================================

init([]) ->
    {ok, #tile_state{}}.

%%%%%% Calls
handle_call(get_population,_From,State) ->
    {reply,State#tile_state.entityDict,State}.


%%%%%% Casts

% needs to capture entities PID
% need to pass that
% check X and Y
handle_cast({summon_entity, Entity}, State) when size(State#tile_state.entityDict) =/= 0 ->
    {ID,{X,Y}} = Entity,
    {noreply,State#tile_state{entityDict = add_unique(ID,{X,Y},State#tile_state.entityDict)}};
handle_cast({summon_entity, Entity}, State) when size(State#tile_state.entityDict) =:= 0 ->
    {ID,{X,Y}} = Entity,
    {noreply,State#tile_state{entityDict = dict:store(ID,{X,Y},State#tile_state.entityDict)}};
%%%%%%%%%%%% SEMI BROKEN
handle_cast({update_entity, Entity, Pos}, State) ->
%%%%%%%%%%%% Any idea how I could do this without having to go to a list? 
%%%%%%%%%%%% Couldn't find a find_in_dict method or something similar
    case in_dict(Entity, dict:to_list(entityDict)) of
        true ->
            {ID,{X,Y}} = Entity,
            {noreply,State#tile_state{entityDict = update_pos(ID,Pos,State#tile_state.entityDict)}};
        false ->
            {noreply,State#tile_state{entityDict = summon_entity(State,Entity)}}
    end.

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
get_population(Pid) ->
    gen_server:call(Pid, get_population).

%%%%%% Casts
summon_entity(Pid, Entity) ->
    gen_server:cast(Pid, {summon_entity, Entity}).

update_entity(Pid, Entity, Pos) ->
    gen_server:cast(Pid, {update_entity, Entity, Pos}).

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

% Updates the position of an entity
%%%%%%%%%%%% THIS IS BROKEN, ANY IDEA WHY?
update_pos(ID, Pos, Dict) ->
   Dict = dict:update(ID, Pos).

% Check if an entity exists currently before updating
in_dict(Entity, [X|Xs]) ->
    {ID,{X,Y}} = Entity,
    if 
        ID =/= X ->
            in_dict(Entity, Xs);
        ID =:= X ->
            true;
        true ->
            false
    end.


% types and specs
% translate list to dicts
