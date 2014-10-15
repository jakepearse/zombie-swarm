-module(swarm_server).
%-behaviour(application).

-export([start/0]).
%-export([stop/1]).

start() ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/websocket", swarm_handler, []}
      ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
             [{env, [{dispatch, Dispatch}]}]).

%stop(_State) ->
%	ok.
