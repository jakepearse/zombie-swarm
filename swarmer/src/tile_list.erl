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
        summon_entity/2]).

-define(SERVER, ?MODULE).

%%%% zombieList : a list of all the Zombies on the current tile
-record(tile_state,{entityList=[]}).

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

%%%%%% calls
handle_call(get_population, _From, State) ->
    {reply, State#tile_state.entityList, State}.


%%%%%% casts
handle_cast({summon_entity, Entity}, State) when State#tile_state.entityList =/= [] ->
    {noreply, State#tile_state{entityList=add_unique(State#tile_state.entityList,Entity)}};
handle_cast({summon_entity, Entity}, State) when State#tile_state.entityList == [] ->
    {noreply, State#tile_state{entityList=State#tile_state.entityList++[Entity]}}.


handle_info(Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================

%%%%%% calls
get_population(Pid) ->
    gen_server:call(Pid, get_population).

%%%%%% casts
summon_entity(Pid, Entity) ->
    gen_server:cast(Pid, {summon_entity, Entity}).


%%%%%% functions
check_list([],_) -> false;
check_list({E1,{X1,Y1}},[{_,{X2,Y2}}|Xs]) ->
    if (X1=/=X2) or (Y1=/=Y2) ->
        check_list({E1,{X1,Y1}}, Xs);
        true -> true
    end.

add_unique(List,Entity) ->
    case check_list(List, Entity) of
        false ->
            List++[Entity];
        true ->
            {E,{X,Y}} = Entity,
            add_unique(List,{E,{X+1,Y+1}})
    end.


% types and specs
% translate list to dicts