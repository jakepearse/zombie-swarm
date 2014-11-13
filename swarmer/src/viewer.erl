-module(viewer).
-author("JakePearse <jp480@kent.ac.uk>").

-behaviour(gen_server).

%%%% API
-export([start_link/0, add_tile/2,get_population/1,update/2,get_tiles/1]).

%%%% gen_server callbacks
-export([code_change/3,handle_cast/2,handle_call/2,handle_call/3,
handle_info/2,init/1,terminate/2]).

-record(state, {id,tileDict=dict:new()}).

%%%%%%=============================================================================
%%%%%% API
%%%%%%=============================================================================

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Start the server.
%%%% @end
%%%%------------------------------------------------------------------------------
%start_link([Id]) -> gen_server:start_link({local,Id}, ?MODULE, [Id], []).
start_link() -> gen_server:start_link(?MODULE, [], []).

%change the data in the viewer
update(Pid,{Tile,Entities}) ->
  gen_server:cast(Pid,{update,{Tile,Entities}}).

% exactly the same as update
add_tile(Pid,{Tile,Entities}) ->
  gen_server:cast(Pid,{add_tile,{Tile,Entities}}).

% returns the entire tileDict
get_population(Pid) ->
  gen_server:call(Pid,get_population).


% returns a list of keys from the tileDict
get_tiles(Pid) ->
  gen_server:call(Pid,get_tiles).

%stop_viewer(_Pid) ->
    %gen_server:terminate(normal).

%%%%%%=============================================================================
%%%%%% gen_server Callbacks
%%%%%%=============================================================================
%init([Id]) -> 
%   {ok, #state{id=Id}}.
init([]) -> 
   {ok, #state{}}. %new state record with default values

%get population - just dump the state out.
handle_call(get_population,_From,State) ->
  {reply,State#state.tileDict,State};

% get tiles - dump the keys in the state dict
handle_call(get_tiles,_From,State) ->
  {reply,dict:fetch_keys(State#state.tileDict),State}.

handle_call(terminate,State) ->
  {stop,normal,State}.

%callback for add_tile
handle_cast({add_tile,{Tile,Entities}},State) ->
{noreply,State#state{tileDict = dict:store(Tile,Entities,State#state.tileDict)}};

% callback for update - replace a key/value pair in the state
handle_cast({update,{Tile,Entities}},State) ->
{noreply,State#state{tileDict = dict:store(Tile,Entities,State#state.tileDict)}}.

% enxpected message
handle_info(Msg,State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
    {noreply,State}.

terminate(normal,_State) ->
    ok.

code_change(_OldVsn, State,_Extra) ->
    {ok,State}.
