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

%%%%%% calls
handle_call(get_population,_From,State) ->
    {reply,State#tile_state.entityDict,State}.


%%%%%% casts

% needs to capture entities PID
% need to pass that
% check X and Y
handle_cast({summon_entity, Entity}, State) ->
    {ID,{X,Y}} = Entity,
    {noreply,State#tile_state{entityDict = dict:store(ID,{X,Y},State#tile_state.entityDict)}}.

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


% types and specs
% translate list to dicts