%%% viewer stores a dictonary of tiles and thier contents, to be used
%%% by entities such as zombies when they want to see whats in range
%%% can return either a full dictionary with viewer:get_population
%%% or a list of which tiles are being tracked with viewer:get_tiles
%%% two functions are provided to update the data in the viewer
%%% viewer:update(Pid,{Tile,list}. and viewer:add_tile:{Tile,List}
%%% both replace the key/value pair identified by Tile

-module(viewer).
-author("JakePearse <jp480@kent.ac.uk>").
% gen_server requirements
-behaviour(gen_server).


% don't need unit tests for these
-export([code_change/3,handle_cast/2,handle_call/2,handle_call/3,
handle_info/2,init/1,terminate/2]).
%-define(SERVER, ?MODULE).
% my stuff

% unit test these
-export([start_link/0,stop_viewer/1, add_tile/2,get_population/1,update/2,get_tiles/1]).

% record for server state, tileDict : a dict to hold tile:entities
-record(state, {id,tileDict=dict:new()}).

% Start link returns a handle to this viewer
%start_link(Id) -> gen_server:start_link({local,Id}, ?MODULE, [Id], []).
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

stop_viewer(_Pid) ->
    gen_server:terminate(normal).
    
% gen server stuff
%init([Id]) -> 
   %{ok, #state{id=Id}}. %new state record with default values

init([]) -> 
   {ok, #state{}}. %new state record with default values

%handle_call/2

%get population - just dump the state out.
handle_call(get_population,_From,State) ->
  {reply,State#state.tileDict,State};
% get tiles - dump the keys in the state dict
handle_call(get_tiles,_From,State) ->
  {reply,dict:fetch_keys(State#state.tileDict),State}.

handle_call(terminate,State) ->
  {stop,normal,State}.

%handle_cast/2

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
