-module(enviroment).
-author("Jake Pearse jp480@kent.ac.uk").

-behaviour(gen_server).

%%% API
-export([start_link/0,make_grid/4,get_grid/1,report/1]).

%%%% internal functions for debugging these can be deleted later
-export([get_state/1,integer_list/1]).

%%%% gen_server callbacks
-export([code_change/3,handle_cast/2,handle_call/2,handle_call/3,
handle_info/2,init/1,terminate/2]).

-define(SERVER, ?MODULE).

-record(state,{
  tileList=[],
  tileSize,
  tileMatrix,
  viewerDict=dict:new(),
  viewerSup
  %tileSup=tile_sup:start_link([]), ---this will only work if tile_sup returns a pid not {ok,Pid}
  }).

%%%%%%=============================================================================
%%%%%% API
%%%%%%=============================================================================

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Start the server.
%%%% @end
%%%%------------------------------------------------------------------------------
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%% TODO %%%
% test if a tile is in range for the viewer
%find_viewers(_,[]) -> [],
%find_viewers(OriginTile,Grid)->
    %[H|T] = Grid,
    %{R1,C1,{X1,Y1},S1} = OriginTile,
    %{R2,C2,{X2,Y2},S2} = H,
    %case (abs(X1-X2) < S1+S2) and (abs(Y1-Y2) < S1+S2) of
    %true ->  

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Get the full tileList form the #state record
%%%% @end
%%%%------------------------------------------------------------------------------
get_grid(Pid) ->
    gen_server:call(Pid,get_grid).

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Get the entire #state for debugging
%%%% @end
%%%%------------------------------------------------------------------------------
get_state(Pid) ->
    gen_server:call(Pid,get_state).

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Make a new tile grid, stored in #state
%%%% @end
%%%%------------------------------------------------------------------------------
make_grid(Pid,Rows,Columns,TileSize) ->
  make_grid(Pid,0,0,0,Rows,Columns,TileSize,[]).
% when the Xcounter and Ycounter are equal to the no of columns and rows
% we're done - stick it in the state
make_grid(Pid,_,X,_,X,_,_TileSize,Grid) ->
  gen_server:cast(Pid,{make_grid,Grid});
make_grid(Pid,TileCounter,RowCounter,ColumnCounter,Rows,Columns,TileSize,Grid) when RowCounter =< Rows -1 ->
  % these methods need to be implemented in tile
  %Tile:set_geometry(RowCounter*Rows,ColumnCounter*Columns,TileSize),
  NewGrid = Grid ++ [make_row(TileCounter,RowCounter, ColumnCounter, Rows, Columns, TileSize)],
  make_grid(Pid,TileCounter,RowCounter +1, ColumnCounter,Rows,Columns,TileSize,NewGrid).

report(Pid) ->
    % eventually this will give the state of the whole enviroment
    % for now it's a dummy to test json stuff
    gen_server:call(Pid,report).
    

%%%%%%=============================================================================
%%%%%% gen_server Callbacks
%%%%%%=============================================================================

init([]) -> 
  {ok,V} = viewer_sup:start_link([]),
   {ok, #state{viewerSup=V}}. %new state record with default values
   
handle_call(report,_From,State) ->
    %here's a dummy callback for json testing
    %Report=lists:filter(fun(X)->is_integer(X) end,lists:flatten(State#state.tileList)),
    Report=integer_list(State#state.tileList),
    {reply,Report,State};

handle_call(get_grid,_From,State) ->
  {reply,State#state.tileList,State};

handle_call(get_state,_From,State) ->
    {reply,State,State}.

handle_call(terminate,State) ->
  {stop,normal,State}.

%% callback for make_grid - tile viewer is assigned here %%
handle_cast({make_grid,Grid},State) ->
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% I'm not sure if we want all the tiles in a 2D matrix or a flat list %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%  The next 2 functions flatten it %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%FlatGrid=lists:flatten(Grid),
%ViewerGrid=lists:map(fun(X) -> {ok,VPid} = supervisor:start_child(State#state.viewerSup,[]), erlang:append_element(X,VPid) end,FlatGrid),
%{noreply,State#state{tileList=ViewerGrid}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% otherwise keep it as a 2d matrix %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

DeepGrid=lists:map(fun(X) ->
    lists:map(fun(Y) ->
        {ok,VPid} = supervisor:start_child(State#state.viewerSup,[]), Y++[VPid] end,X)
    end,Grid),
{noreply,State#state{tileList=DeepGrid}}.
  
handle_info(Msg,State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
    {noreply,State}.

terminate(normal,_State) ->
    ok.

code_change(_OldVsn, State,_Extra) ->
    {ok,State}.

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================

make_row(TileCounter,RowCounter,ColumnCounter,_Rows,Columns,TileSize) ->
  make_row(TileCounter,RowCounter,ColumnCounter,_Rows,Columns,TileSize,[]).
make_row(_TileCounter,_RowCounter,ColumnCounter,_Rows,Columns,_TileSize,Row) when ColumnCounter > Columns -1 -> Row;
make_row(TileCounter,RowCounter,ColumnCounter,_Rows,Columns,TileSize,Row) ->
  %::look:: we need to sort out the thing that initalises the state from the supervisor args
  %make_row(RowCounter,ColumnCounter +1,_Rows,Columns,TileSize,Row++[supervisor:start_child(State#state.tileSup,[RowCounter,ColumnCounter,TileSize])].
  make_row(TileCounter +1, RowCounter,ColumnCounter +1,_Rows,Columns,TileSize,Row++[[RowCounter,ColumnCounter,[ColumnCounter*TileSize,RowCounter*TileSize],TileSize]]).

integer_list([]) -> [];
integer_list([X|Xs]) ->
  [lists:filter(fun(Element) -> is_integer(Element) end,X)] ++ integer_list(Xs).
