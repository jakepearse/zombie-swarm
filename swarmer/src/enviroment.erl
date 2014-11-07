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
  viewerDict=dict:new()
  tileSup=tile_sup:start_link([]),
  viewerSup=viewer_sup:start_link([]),
  }).

% start_link, you know what to do
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

make_grid(Pid,Rows,Columns,TileSize) ->
  make_grid(Pid,0,0,Rows,Columns,TileSize,[]).

% when the Xcounter and Ycounter are equal to the no of columns and rows
% we're done - stick it in the state
make_grid(Pid,X,_,X,_,_TileSize,Grid) ->
  gen_server:cast(Pid,{make_grid,Grid});

make_grid(Pid,RowCounter,ColumnCounter,Rows,Columns,TileSize,Grid) when RowCounter =< Rows -1 ->
  % these methods need to be implemented in tile
  %Tile:set_geometry(RowCounter*Rows,ColumnCounter*Columns,TileSize),
  NewGrid = Grid ++ [make_row(RowCounter, ColumnCounter, Rows, Columns, TileSize)],
  make_grid(Pid,RowCounter +1, ColumnCounter,Rows,Columns,TileSize,NewGrid).


make_row(RowCounter,ColumnCounter,_Rows,Columns,TileSize) ->
  make_row(RowCounter,ColumnCounter,_Rows,Columns,TileSize,[]).
make_row(RowCounter,ColumnCounter,_Rows,Columns,TileSize,Row) when ColumnCounter > Columns -1 -> Row;
make_row(RowCounter,ColumnCounter,_Rows,Columns,TileSize,Row) ->
  %look we need to sort out the thing that initalises the state from the supervisor args
  make_row(RowCounter,ColumnCounter +1,_Rows,Columns,TileSize,Row++[supervisor:start_child(State#state.tileSup,[RowCounter,ColumnCounter])].

get_grid(Pid) ->
  gen_server:call(Pid,get_grid).

new_tile() ->
  {ok,TilePid} = tile:start_link(),
  %TilePid.

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

%callback for add_tile
handle_cast({make_grid,Grid},State) ->
{noreply,State#state{tileList=Grid}}.
