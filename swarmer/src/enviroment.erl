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
  viewerPropList,
  viewerSup,
  tileSup
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
  gen_server:cast(Pid,{make_grid,{Rows,Columns,TileSize}}).



report(Pid) ->
    % eventually this will give the state of the whole enviroment
    % for now it's a dummy to test json stuff
    gen_server:call(Pid,report).
    

%%%%%%=============================================================================
%%%%%% gen_server Callbacks
%%%%%%=============================================================================

init([]) -> 
  {ok,V} = viewer_sup:start_link([]),
  {ok,T} = tile_sup:start_link([]),
   {ok, #state{viewerSup=V,tileSup=T}}. %new state record with default values
   
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
handle_cast({make_grid,{Rows,Columns,TileSize}},State) ->
Grid = populate_grid(State#state.tileSup,Rows,Columns,TileSize),
Viewers=add_viewers(State#state.viewerSup,Grid),
%setNeighbours(Viewers),
{noreply,State#state{tileList=Grid,viewerPropList=Viewers}}. 


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

integer_list([]) -> [];
integer_list([X|Xs]) ->
  lists:filter(fun(Element) -> is_integer(Element) end,X) ++ integer_list(Xs).
  
make_row(TileSup,RowCounter,Columns,ColumnCounter,TileSize) ->
  make_row(TileSup,RowCounter,Columns,ColumnCounter,TileSize,[]).
make_row(_TileSup,_RowCounter,Columns,ColumnCounter,_TileSize,Row) when ColumnCounter > Columns -1 ->
  Row;
make_row(TileSup,RowCounter,Columns,ColumnCounter,TileSize,Row) ->
  {ok,Tile} = supervisor:start_child(TileSup,
                                        [RowCounter*TileSize,
                                        ColumnCounter*TileSize,
                                        TileSize]),
  make_row(TileSup,RowCounter,Columns,ColumnCounter +1,TileSize,
            Row++[Tile]).
  
 
populate_grid(TileSup,Rows,Columns,TileSize) ->
  populate_grid(TileSup,0,Rows,Columns,TileSize,[]).
populate_grid(_TileSup,RowCounter,Rows,_Columns,_TileSize,Grid) when RowCounter > Rows -1 ->
  Grid;
populate_grid(TileSup,RowCounter,Rows,Columns,TileSize,Grid) ->
  populate_grid(TileSup,RowCounter +1,Rows,Columns,TileSize,Grid++make_row(TileSup,RowCounter,Columns,0,TileSize)).


%add_viewers(Sup,Grid) ->
 %add_viewers(Sup,Grid,[]).
%add_viewers(_,[],Viewers) ->
  %Viewers;
%add_viewers(Sup,Grid,Viewers) ->
  %[H|T]=Grid,
  %{ok,V}=supervisor:start_child(Sup,[]),
  %tile:set_viewer(H,V),
  %add_viewers(Sup,T,Viewers ++ [{H,V}]).

%setNeighbours(Viewers) ->
  %[{Tile,Viewer}|T] = Viewers,
  %find_viewers(Tile,Viewers)
  

%%test if a tile is in range for the viewer
%find_viewers(_,[]) -> [],
%find_viewers(OriginTile,Grid)->
    %{X,Y,XL,YL} = tile:get_geometry(OriginTile),
    %Size = abs(XL-X),
    %[H|T] = Grid,
    %{R2,C2,{X2,Y2},S2} = H,
    %%case (abs(X1-X2) < S1+S2) and (abs(Y1-Y2) < S1+S2) of
    %%true ->  
