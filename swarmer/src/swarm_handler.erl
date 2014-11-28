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

-record(state, {}).

% take any request from tcp or http and push it to websockets
init({tcp,http},_Req,_Opts) ->
	%error_logger:error_report("init started"),
  {upgrade, protocol, cowboy_websocket}.

% if its any other protocol it should hit here
%handle(_, State) ->
%  {ok, Req2} = cowboy_req:reply(404, [{'Content-Type', <<"text/html">>}]),
%  {ok, Req2, State}.

websocket_init(_TransportName, Req, _Opts) ->
  %error_logger:error_report("ws init started"),

  {ok, Req, #state{}}.
        
websocket_handle({text, Json}, Req, State) ->
    Parsed = jsx:decode(Json),
    Type = proplists:get_value(<<"type">>,Parsed),
    case Type of
      <<"setup">> ->
        BinArrity = proplists:get_value(<<"arrity">>,Parsed),
        Arrity = binary_to_integer(BinArrity),
        BinTileSize = proplists:get_value(<<"tileSize">>,Parsed),
        TileSize = binary_to_integer(BinTileSize),
        enviroment:make_grid(Arrity,Arrity,TileSize),
        Status=jsx:encode(enviroment:get_grid_info()),
        {reply, [{text,Status}], Req, State};
     
     <<"swarm">> ->
        BinSize = proplists:get_value(<<"size">>,Parsed),
        Size = binary_to_integer(BinSize),
        enviroment:set_swarm(Size),
        RawReport = enviroment:report(),
        error_logger:error_report(RawReport),
        Report = jsx:encode(RawReport),
        {reply, [{text,Report}], Req, State};

      <<"report">> ->
        Report = jsx:encode(enviroment:report()),
        {reply, [{text,Report}], Req, State};
      
      _ ->
    error_logger:error_report({"Unknown message type", Type}),
              {noreply, Req, State}
    end;

websocket_handle({text, <<"update">>}, Req, State)  ->
  %error_logger:error_report("ws2 handle started"),
  Report=mochijson2:encode(enviroment:report()),
  %error_logger:error_report(Report),
  {reply, [{text, Report}], Req, State}; % << "report", Report/binary >>

websocket_handle({text, Msg}, Req, State) ->
  %error_logger:error_report(Msg),
  Report=mochijson2:encode(enviroment:report()),
  {reply, [{text, Report}], Req, State}; % << "report", Report/binary >>

websocket_handle(_Any, Req, State) ->
  %error_logger:error_report("ws handle1 started"),
  {reply, [{text, << "what the duce?" >>}], Req, State}.

websocket_info(_Info, Req, State) ->
  %error_logger:error_report("ws info started"),
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  %error_logger:error_report("ws term started"),
  ok.

terminate(_Reason, _Req, _State) ->
  error_logger:error_report("someother term started"),
  ok.
