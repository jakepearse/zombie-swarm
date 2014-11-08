-module(enviroment).
-behaviour(gen_server).
% don't need unit tests for these
-export([code_change/3,handle_cast/2,handle_call/2,handle_call/3,
handle_info/2,init/1,terminate/2]).

-define(SERVER, ?MODULE).
% my stuff

-export([start_link/0,make_grid/4,get_grid/1]).

-record(state,{
  tileList=[],
  tileSize=25,
  tileMatrix=3,
  viewerDict=dict:new(),
  %tileSup=tile_sup:start_link([]),
  viewerSup=viewer_sup:start_link([])
  }).

% start_link, you know what to do
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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


make_row(TileCounter,RowCounter,ColumnCounter,_Rows,Columns,TileSize) ->
  make_row(TileCounter,RowCounter,ColumnCounter,_Rows,Columns,TileSize,[]).
make_row(_TileCounter,_RowCounter,ColumnCounter,_Rows,Columns,_TileSize,Row) when ColumnCounter > Columns -1 -> Row;
make_row(TileCounter,RowCounter,ColumnCounter,_Rows,Columns,TileSize,Row) ->
  %::look:: we need to sort out the thing that initalises the state from the supervisor args
  %make_row(RowCounter,ColumnCounter +1,_Rows,Columns,TileSize,Row++[supervisor:start_child(State#state.tileSup,[RowCounter,ColumnCounter,TileSize])].
  make_row(TileCounter +1, RowCounter,ColumnCounter +1,_Rows,Columns,TileSize,Row++[{RowCounter,ColumnCounter,{ColumnCounter*TileSize,RowCounter*TileSize},TileSize}]).

% test if a tile is in range for the viewer
%find_viewers(_,[]) -> [],
%find_viewers(OriginTile,Grid)->
    %[H|T] = Grid,
    %{R1,C1,{X1,Y1},S1} = OriginTile,
    %{R2,C2,{X2,Y2},S2} = H,
    %case (abs(X1-X2) < S1+S2) and (abs(Y1-Y2) < S1+S2) of
    %true ->  

get_grid(Pid) ->
  gen_server:call(Pid,get_grid).

%new_tile() ->
  %{ok,TilePid} = tile:start_link().
  %%TilePid.

% enxpected message
handle_info(Msg,State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
    {noreply,State}.

terminate(normal,_State) ->
    ok.

code_change(_OldVsn, State,_Extra) ->
    {ok,State}.

% gen_server callbacks
% gen server stuff
init([]) -> 
   {ok, #state{}}. %new state record with default values


%junk form viewer.erl
% get tiles - dump the keys in the state dict
handle_call(get_grid,_From,State) ->
  {reply,State#state.tileList,State}.

handle_call(terminate,State) ->
  {stop,normal,State}.

%handle_cast/2
%fun(Tile) -> append_element(Tile,[supervisor:start_child(State#state.viewerSup,[])] end)

%callback for add_tile
handle_cast({make_grid,Grid},State) ->
FlatGrid=lists:flatten(Grid),
ViewerGrid=lists:map(fun(X) -> erlang:append_element(X,[supervisor:start_child(State#state.viewerSup,[])]) end,FlatGrid),
{noreply,State#state{tileList=ViewerGrid}}.
