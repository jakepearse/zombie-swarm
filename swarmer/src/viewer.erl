-module(viewer).
-author("JakePearse <jp480@kent.ac.uk>").

-behaviour(gen_server).

%%%% API
-export([start_link/0,get_population/1,update_population/2]).

%%%% gen_server callbacks
-export([code_change/3,handle_cast/2,handle_call/3,
handle_info/2,init/1,terminate/2]).

-record(state, {tile_map=maps:new(),
                population_map=maps:new() :: maps:map(),
                zombieList :: list(),
                humanList :: list()}).

%%%%%%==========================================================================
%%%%%% API
%%%%%%==========================================================================

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Start the server.
%%%% @end
%%%%----------------------------------------------------------------------------
start_link() -> gen_server:start_link(?MODULE, [], []).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Update population of the neighbourhood.
%%%% @end
%%%%----------------------------------------------------------------------------
update_population(Pid, {Tile,Entities}) ->
    gen_server:cast(Pid,{update_population,Tile,Entities}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Returns the population of the neighbourhood.
%%%% @end
%%%%----------------------------------------------------------------------------
get_population(Pid) ->
    gen_server:call(Pid,get_population).

%%%%%%==========================================================================
%%%%%% gen_server Callbacks
%%%%%%==========================================================================
init([]) -> 
   {ok, #state{}}. %new state record with default values

handle_cast({update_population, Tile, Entities}, #state{population_map = PopMap} = State) ->
    {noreply, State#state{population_map = maps:put(Tile,Entities,PopMap)}}.

%get population - just dump the state out.
handle_call(get_population,_From,State) ->
    {reply,State#state.population_map,State}.

% enxpected message
handle_info(Msg,State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
        {noreply,State}.

terminate(normal,_State) ->
    ok.

code_change(_OldVsn, State,_Extra) ->
    {ok,State}.

% Things Viewer Needs:
%     get zombieList
%     get humanList
