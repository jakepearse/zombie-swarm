-module(enviroment).
-author("Jake Pearse jp480@kent.ac.uk").

-behaviour(gen_server).

-include_lib("include/swarmer.hrl").

%%% API
-export([start_link/0,make_grid/3,get_grid_info/0,report/0, 
         pause_entities/0, unpause_entities/0]).

%%%% internal functions for debugging these can be deleted later
-export([get_state/0,set_swarm/1]).

%%%% gen_server callbacks
-export([code_change/3,handle_cast/2,handle_call/2,handle_call/3,
handle_info/2,init/1,terminate/2]).

-define(SERVER, ?MODULE).

-record(state,{
% map viewer PID's to respective tiles
  viewerPropList,
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
start_link() -> 
    gen_server:start_link({local,?MODULE},?MODULE, [], []).

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Get general information about the grid
%%%% @end
%%%%------------------------------------------------------------------------------
get_grid_info() ->
    gen_server:call(?MODULE,grid_info).

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

%%%%------------------------------------------------------------------------------
%%%% @doc
%% Pauses all running entities
%%%% @end
%%%%------------------------------------------------------------------------------
pause_entities() -> 
  do_pause_entities().

%%%%------------------------------------------------------------------------------
%%%% @doc
%% Pauses all running entities
%%%% @end
%%%%------------------------------------------------------------------------------
unpause_entities() -> 
  do_unpause_entities().

%%%%%%=============================================================================
%%%%%% gen_server Callbacks
%%%%%%=============================================================================

init([]) -> 
   {ok, #state{}}. %new state record with default values

%%% calls   
handle_call(report,_From,State) ->
    Report = make_report(), 
    {reply,Report,State};


handle_call(grid_info,_From,State) ->
  Rows = State#state.rows,
  Columns = State#state.columns,
  Size = State#state.tileSize,
  {reply,[{<<"rows">>,Rows},{<<"columns">>,Columns},{<<"tileSize">>,Size}],State};

handle_call(get_state,_From,State) ->
    {reply,State,State}.

handle_call(terminate,State) ->
  {stop,normal,State}.

% casts

handle_cast({make_grid,{Rows,Columns,TileSize}},State) ->
  %kill entities
  %Kill tiles
  %Kill viewers
  Grid = populate_grid(Rows,Columns,TileSize),
  Viewers=add_viewers(Grid),
  
  make_neighbourhood(Grid,Viewers),
  {noreply,State#state{rows=Rows,columns=Columns,tileSize=TileSize,viewerPropList=Viewers}};

handle_cast({swarm,Num},State) ->
  %kill all entities
  create_swarm(State,Num),
  {noreply,State}.


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
make_row(RowCounter,Columns,ColumnCounter,TileSize) ->
  make_row(RowCounter,Columns,ColumnCounter,TileSize,[]).
make_row(_RowCounter,Columns,ColumnCounter,_TileSize,Row) when ColumnCounter > Columns -1 ->
  Row;
make_row(RowCounter,Columns,ColumnCounter,TileSize,Row) ->
  Name = list_to_atom("tile" ++  "X" ++ integer_to_list(ColumnCounter) ++  "Y" ++ integer_to_list(RowCounter)),
  {ok,_} = supervisor:start_child(tile_sup,
                                        [Name,
                                        ColumnCounter*TileSize,
                                        RowCounter*TileSize,
                                        TileSize]),
  make_row(RowCounter,Columns,ColumnCounter +1,TileSize, Row++[Name]).
  
%%% populate could be misleading, it means populate a grid with tiles %%%
populate_grid(Rows,Columns,TileSize) ->
  populate_grid(0,Rows,Columns,TileSize,[]).
populate_grid(RowCounter,Rows,_Columns,_TileSize,Grid) when RowCounter > Rows -1 ->
  Grid;
populate_grid(RowCounter,Rows,Columns,TileSize,Grid) ->
  populate_grid(RowCounter +1,Rows,Columns,TileSize,Grid++make_row(RowCounter,Columns,0,TileSize)).

%%% take a list of tiles and add viewers to each one
add_viewers(Grid) ->
 add_viewers(Grid,[]).
add_viewers([],Viewers) ->
  Viewers;
add_viewers(Grid,Viewers) ->
  [H|T]=Grid,
  {ok,V}=supervisor:start_child(viewer_sup,[]),
  tile:set_viewer(H,V),
  add_viewers(T,Viewers ++ [{H,V}]).


%% Spawns Num randomly positioned zombies
create_swarm(#state{tileSize = TileSize, columns = Columns, rows = Rows} = State,Num) ->
    GridXSize=TileSize*Columns,
    GridYSize=TileSize*Rows,
    lists:foreach(
        fun(_) ->
            Xpos = random:uniform(GridXSize),
            Ypos= random:uniform(GridYSize),
            {Tile,Viewer} = get_tile(Xpos,Ypos,State),
            {ok,Zombie}=supervisor:start_child(zombie_sup,[Xpos,Ypos,Tile,TileSize,Columns,Rows,Viewer,1,0,300]),
            %temporary fix
            zombie_fsm:start(Zombie)
        end,lists:seq(1,Num)).

%% search for a tile by X,Y in the viewerPropList
get_tile(Xpos,Ypos,#state{viewerPropList = ViewerPropList, tileSize = TileSize}) -> 
  Tile = list_to_atom("tile" ++  "X" ++ integer_to_list(Xpos div TileSize) ++  "Y" ++ integer_to_list(Ypos div TileSize)),
  Viewer = proplists:get_value(Tile,ViewerPropList),
  {Tile,Viewer}.

%% Makes a report for the client.
%% This report contains a list of lists, built from polling the zombie_sup
%% for current position of all it's children.
make_report() ->
    lists:filtermap(
        fun({_Id, Pid, _Type, _Modules}) ->
            case zombie_fsm:get_state(Pid) of
                {ok, #entity_status{id = ID, x = X, y = Y} = _EntityStatus} ->
                   {true, [{id, list_to_binary(pid_to_list(ID))}, {x, X}, {y, Y}]};
                _ ->
                    false
            end
        end, supervisor:which_children(zombie_sup)).
%% ADD OTHER SUPERVISORS IF MORE THAN JUST ZOMBIES

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

do_pause_entities() ->
    lists:foreach(
        fun({_Id, Pid, _Type, _Modules}) ->
            zombie_fsm:pause(Pid)
        end, supervisor:which_children(zombie_sup)).
    %% ADD OTHER SUPERVISORS IF MORE THAN JUST ZOMBIES

do_unpause_entities() ->
    lists:foreach(
        fun({_Id, Pid, _Type, _Modules}) ->
            zombie_fsm:unpause(Pid)
        end, supervisor:which_children(zombie_sup)).
    %% ADD OTHER SUPERVISORS IF MORE THAN JUST ZOMBIES
