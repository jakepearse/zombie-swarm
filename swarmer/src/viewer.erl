-module(viewer).
-author("JakePearse <jp480@kent.ac.uk>").

-behaviour(gen_server).

%%%% API
-export([start_link/0,get_zombies/1, get_humans/1,
         update_zombies/2, update_humans/2]).

%%%% gen_server callbacks
-export([code_change/3,handle_cast/2,handle_call/3,
handle_info/2,init/1,terminate/2]).

-record(state, {tile_map=maps:new(),
                zombie_map = maps:new(),
                human_map = maps:new()}).

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
update_zombies(Pid, {Tile,Entities}) ->
    gen_server:cast(Pid,{update_zombies,Tile,Entities}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Update population of the neighbourhood.
%%%% @end
%%%%----------------------------------------------------------------------------
update_humans(Pid, {Tile,Entities}) ->
    gen_server:cast(Pid,{update_humans,Tile,Entities}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Returns the population of the neighbourhood.
%%%% @end
%%%%----------------------------------------------------------------------------
get_zombies(Pid) ->
    gen_server:call(Pid,get_zombies).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Returns the population of the neighbourhood.
%%%% @end
%%%%----------------------------------------------------------------------------
get_humans(Pid) ->
    gen_server:call(Pid,get_humans).

%%%%%%==========================================================================
%%%%%% gen_server Callbacks
%%%%%%==========================================================================
init([]) -> 
   {ok, #state{}}. %new state record with default values

handle_cast({update_zombies, Tile, Entities}, #state{zombie_map = Zmap} = State) ->
    {noreply, State#state{zombie_map = maps:put(Tile,Entities,Zmap)}};

handle_cast({update_humans, Tile, Entities}, #state{human_map = Hmap} = State) ->
    {noreply, State#state{human_map = maps:put(Tile,Entities,Hmap)}}.

handle_call(get_zombies,_From,State) ->
    Map = lists:flatten(maps:values(State#state.zombie_map)),
    {reply,Map,State};

handle_call(get_humans,_From,State) ->
    Map = lists:flatten(maps:values(State#state.human_map)),
    {reply,Map,State}.

% enxpected message
handle_info(Msg,State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
        {noreply,State}.

terminate(normal,_State) ->
    ok.

code_change(_OldVsn, State,_Extra) ->
    {ok,State}.
