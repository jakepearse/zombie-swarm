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

-record(state, {enviroment=0}).

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

  {ok, Req, #state{}}.

%websocket_handle({text, Msg}, Req, State) ->
  %error_logger:error_report("ws handle1 started"),
  %{reply, [{text, << "erlang responding to ", Msg/binary >>}], Req, State};

websocket_handle({text, <<"initial">>}, Req, State) when State#state.enviroment =/= 0->
    error_logger:error_report("ws handle1 when started"),
    Report=mochijson2:encode(enviroment:report(State#state.enviroment)),
        {reply, [{text, Report}], Req, State}; % << "report", Report/binary >>
        
websocket_handle({text, <<"initial">>}, Req, State) ->
    error_logger:error_report("ws handle1 started"),
    {ok,E}=enviroment:start_link(),
    enviroment:make_grid(E,3,3,25),
    enviroment:set_swarm(E,50),
    Report=mochijson2:encode(enviroment:report(E)),
    {reply, [{text, Report}], Req, State#state{enviroment=E}}; % << "report", Report/binary >>

%  error_logger:error_report(Report),

websocket_handle({text, <<"update">>}, Req, State) ->
  error_logger:error_report("ws2 handle started"),
  Report=mochijson2:encode(enviroment:report(State#state.enviroment)),
  {reply, [{text, Report}], Req, State}; % << "report", Report/binary >>

websocket_handle({text, Msg}, Req, State) ->
  error_logger:error_report(Msg),
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
