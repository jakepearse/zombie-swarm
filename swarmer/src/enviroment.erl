-module(enviroment).
-author("Jake Pearse jp480@kent.ac.uk").

-behaviour(gen_server).

%%% API
-export([start_link/0,make_grid/3,get_grid/0,report/0]).

%%%% internal functions for debugging these can be deleted later
-export([get_state/0,set_swarm/1]).

%%%% gen_server callbacks
-export([code_change/3,handle_cast/2,handle_call/2,handle_call/3,
handle_info/2,init/1,terminate/2]).

-define(SERVER, ?MODULE).

-record(state,{
% a canonical list of tile PID's
  tileList=[],
% a canonical list of entities
  swarm = [],
% map viewer PID's to respective tiles
  viewerPropList,
% handle to viewer supervisor
  viewerSup,
% handle to tile supervisor
  tileSup,
% handle to zombie supervisor
  zombieSup,
% Number of tile rows
  rows,
% number of tile columns
  columns,
% size of each tile  
  tileSize
  }).

%%%%%%=============================================================================
%%%%%% API
%%%%%%=============================================================================

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Start the server.
%%%% @end
%%%%------------------------------------------------------------------------------
start_link() -> gen_server:start_link({local,?MODULE},?MODULE, [], []).


%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Get the full tileList form the #state record
%%%% @end
%%%%------------------------------------------------------------------------------
get_grid() ->
    gen_server:call(?MODULE,get_grid).

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Get the entire #state for debugging
%%%% @end
%%%%------------------------------------------------------------------------------
get_state() ->
    gen_server:call(?MODULE,get_state).

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Make a new tile grid, stored in #state
%%%% @end
%%%%------------------------------------------------------------------------------
make_grid(Rows,Columns,TileSize) ->
  gen_server:cast(?MODULE,{make_grid,{Rows,Columns,TileSize}}).

%%%%------------------------------------------------------------------------------
%%%% @doc
%% retrive the state of the whole enviroment
%%%% @end
%%%%------------------------------------------------------------------------------
report() ->
    gen_server:call(?MODULE,report).

%%%%------------------------------------------------------------------------------
%%%% @doc
%% create Num zombies and add add them to the tiles
%%%% @end
%%%%------------------------------------------------------------------------------
set_swarm(Num) -> 
  gen_server:cast(?MODULE,{swarm,Num}).



%%%%%%=============================================================================
%%%%%% gen_server Callbacks
%%%%%%=============================================================================

init([]) -> 
  {ok,V} = viewer_sup:start_link([]),
  {ok,T} = tile_sup:start_link([]),
  {ok,Z} = zombie_sup:start_link([]),
   {ok, #state{viewerSup=V,tileSup=T, zombieSup=Z}}. %new state record with default values

%%% calls

handle_call(report,_From,State) ->
    Report = make_report(State#state.tileList), 
    {reply,Report,State};

handle_call(get_grid,_From,State) ->
  {reply,State#state.tileList,State};

handle_call(get_state,_From,State) ->
    {reply,State,State}.

handle_call(terminate,State) ->
  {stop,normal,State}.

% casts

handle_cast({make_grid,{Rows,Columns,TileSize}},State) ->
  Grid = populate_grid(State#state.tileSup,Rows,Columns,TileSize),
  Viewers=add_viewers(State#state.viewerSup,Grid),
  
  _=make_neighbourhood(Grid,Viewers),
  {noreply,State#state{tileList=Grid,viewerPropList=Viewers,rows=Rows,columns=Columns,tileSize=TileSize}};

handle_cast({swarm,Num},State) ->
  Swarm=create_swarm(State,Num),
  {noreply,State#state{swarm=Swarm}}.


% other gen_server stuff

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


%%%% called by populate grid to make a row of tiles %%%%%%%
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
  
%%% populate could be misleading, it means populate a grid with tiles %%%
populate_grid(TileSup,Rows,Columns,TileSize) ->
  populate_grid(TileSup,0,Rows,Columns,TileSize,[]).
populate_grid(_TileSup,RowCounter,Rows,_Columns,_TileSize,Grid) when RowCounter > Rows -1 ->
  Grid;
populate_grid(TileSup,RowCounter,Rows,Columns,TileSize,Grid) ->
  populate_grid(TileSup,RowCounter +1,Rows,Columns,TileSize,Grid++make_row(TileSup,RowCounter,Columns,0,TileSize)).

%%% take a list of tiles and add viewers to each one
add_viewers(Sup,Grid) ->
 add_viewers(Sup,Grid,[]).
add_viewers(_,[],Viewers) ->
  Viewers;
add_viewers(Sup,Grid,Viewers) ->
  [H|T]=Grid,
  {ok,V}=supervisor:start_child(Sup,[]),
  tile:set_viewer(H,V),
  add_viewers(Sup,T,Viewers ++ [{H,V}]).


%% Spawns Num randomly positioned zombies
create_swarm(State,Num) ->
  create_swarm(State,Num,[]).

create_swarm(_State,0,List) -> List;

create_swarm(State,Num,List) ->
  GridXSize=State#state.tileSize*State#state.columns,
  GridYSize=State#state.tileSize*State#state.rows,
  Xpos = random:uniform(GridXSize),
  Ypos= random:uniform(GridYSize),
  {Tile,Viewer} = get_tile(Xpos,Ypos,State#state.tileList,State),
  % zombie now takes {X,Y,Tile,Viewer,Speed,Bearing,Timeout}
  {ok,Zombie}=supervisor:start_child(State#state.zombieSup,[Xpos,Ypos,Tile,Viewer,0,0,300]),
  tile:summon_entity(Tile,{Zombie,{Xpos,Ypos}}),
  create_swarm(State,Num-1,List++[Zombie]).


%% search for a tile by X,Y in the viewerPropList
get_tile(_Xpos,_Ypos,[],State) -> 
  [{Tile,Viewer}|_T] = State#state.viewerPropList,
  {Tile,Viewer};

get_tile(Xpos,Ypos,TL,State) ->
  [H|T] = TL,
  case in_tile(Xpos,Ypos,tile:get_geometry(H)) of
    true -> {H,proplists:get_value(H,State#state.viewerPropList)};
    false -> get_tile(Xpos,Ypos,T,State)
  end.

%% Boolean check of wheter a set of X,Y is within the tile Geom
in_tile(Xpos,Ypos,Geom) ->
 {Xt,Yt,Xl,Yl,_Size}=Geom,
 ((Xpos >= Xt) and (Xpos =< Xl)) and ((Ypos >= Yt) and (Ypos =< Yl)).

%% Build a list of the population of every tile
make_report(TileList) -> 
  make_report(TileList, []).

make_report([],PopList) -> 
  PopList;
make_report(TileList,PopList) ->
  [X|Xs] = TileList,
  NewPopList = PopList ++ tile:get_population(X),
  make_report(Xs,NewPopList).


make_neighbourhood(TileList,ViewerPropList) ->
  ViewerGeomList = setup_neighbours(ViewerPropList),
  do_make_neighbourhood(TileList,ViewerGeomList).
do_make_neighbourhood([],_) -> ok;
do_make_neighbourhood(TileList,ViewerGeomList) ->
  [T|Ts] = TileList,
  {Xo,Yo,_,_,_Size} = tile:get_geometry(T),
  tile:set_neighbours(T,get_neighbours(Xo,Yo,ViewerGeomList)),
  do_make_neighbourhood(Ts,ViewerGeomList).
  
setup_neighbours(ViewerPropList) -> 
  %reformat the viewer propslist inot one with geometry
  lists:map(fun({T,V})-> {tile:get_geometry(T),V} end, ViewerPropList).



get_neighbours(Xo,Yo,ViewersWithGeometry) ->
  get_neighbours(Xo,Yo,ViewersWithGeometry,[]).
  
get_neighbours(_,_,[],NeighbourList) -> NeighbourList;
get_neighbours(Xo,Yo,ViewersWithGeometry,NeighbourList) ->
  [V|Vs]=ViewersWithGeometry,
  {{X,Y,_,_,Size},Viewer} = V,
  case test_neighbour (Xo,Yo,X,Y,Size) of
  true ->
    NewList =NeighbourList ++ [Viewer],
    get_neighbours(Xo,Yo,Vs,NewList);
  false -> get_neighbours(Xo,Yo,Vs,NeighbourList)
  end.


test_neighbour(Xo,Yo,X,Y,Size) ->
      (X =:= Xo + Size) or (X =:= Xo) or (X =:= Xo - Size)
      andalso
      (Y =:= Yo + Size) or (Y =:= Yo) or (Y =:= Yo - Size).


