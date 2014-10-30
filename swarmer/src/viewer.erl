-module(viewer).

% gen_server requirements
-behaviour(gen_server).

-export([code_change/3,handle_call/2,handle_cast/2,
handle_info/2,init/1,terminate/2]).

% my stuff

-export([start_link/0, add_tile/2,get_population/1,update/2,get_tiles/1]).


% Start link returns a handle to this viewer
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% update change the data in the viewer
% this should be a cast, it async and no response is required
update(Pid,{Tile,[Entities]}) ->
  gen_server:cast(Pid,{update,{Tile,[Entities]}}).

add_tile(Pid,{Tile,[Entities]}) ->
  gen_server:cast(Pid,{add_tile,{Tile,[Entities]}}).

get_population(Pid) ->
  gen_server:call(Pid,get_population).
% get population, dump the data in this viewer
% this should be a gen_server:call because we expect a direct reply


% get tiles find out who is dumping here
get_tiles(Pid) ->
  gen_server:call(Pid,get_tiles).

% gen server stuff

init([]) -> 
    {ok,dict:new()}. % can I use this dict as the state?

%handle_call/2

handle_call(get_population,State) ->
  {reply,State,State};
handle_call(get_tiles,State) ->
  {reply,dict:fetch_keys(State),State};
handle_call(terminate,State) ->
  {stop,normal,ok,State}.
%handle_cast/2
handle_cast(Pid,{update,{Tile_name,[Entities]}}) ->
  {noreply,dict:store(Tile_name,[Entities])};
  % hmm, these look suspisiously similar!!
handle_cast(Pid,{add_tile,{Tile_name,[Entities]}}) ->
  {noreply,dict:store(Tile_name,[Entities])}.

handle_info(Msg,State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
    {noreply,State}.

terminate(normal,State) ->
    ok.

code_change(_OldVsn, State,_Extra) ->
    {ok,State}.
