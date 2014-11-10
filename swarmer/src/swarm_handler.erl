-module(swarm_handler).

%-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
%-export([handle/2]).
-export([terminate/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {enviroment}).

% take any request from tcp or http and push it to websockets
init({tcp,http},_Req,_Opts) ->
	error_logger:error_report("init started"),
  {upgrade, protocol, cowboy_websocket}.

% if its any other protocol it should hit here
%handle(_, State) ->
%  {ok, Req2} = cowboy_req:reply(404, [{'Content-Type', <<"text/html">>}]),
%  {ok, Req2, State}.

websocket_init(_TransportName, Req, _Opts) ->
  error_logger:error_report("ws init started"),
  {ok,E}=enviroment:start_link(),
  enviroment:make_grid(E,3,3,25),
  {ok, Req, #state{enviroment=E}}.

%websocket_handle({text, Msg}, Req, State) ->
  %error_logger:error_report("ws handle1 started"),
  %{reply, [{text, << "erlang responding to ", Msg/binary >>}], Req, State};
  
websocket_handle({text, Msg}, Req, State) ->
  error_logger:error_report("ws handle1 started"),
  Report=mochijson2:encode(enviroment:report(State#state.enviroment)),
  {reply, [{text, Report}], Req, State}; % << "report", Report/binary >>


websocket_handle(_Any, Req, State) ->
  error_logger:error_report("ws handle1 started"),
  {reply, [{text, << "what the duce?" >>}], Req, State}.

websocket_info(_Info, Req, State) ->
  error_logger:error_report("ws info started"),
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  error_logger:error_report("ws term started"),
  ok.

terminate(_Reason, _Req, _State) ->
  error_logger:error_report("someother term started"),
  ok.
