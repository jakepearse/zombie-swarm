-module(swarm_server).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_,_) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/websocket", swarm_handler, []}
      ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
             [{env, [{dispatch, Dispatch}]}]),
  swarm_sup:start_link([]).
stop(_State) ->
	ok.
