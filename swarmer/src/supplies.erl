-module(supplies).
-author("Joe Mitchard jm710@kent.ac.uk").

-behaviour(gen_server).

%%%% API
-export([start_link/6, 
			get_type/1, 
			get_state/1,
			picked_up/1]).

%%%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state,{	id,
				type,
				item,
				viewer,
				tile,
				x,
				y}).

%%%%%%=============================================================================
%%%%%% API
%%%%%%=============================================================================

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Start the server.
%%%% @end
%%%%------------------------------------------------------------------------------
start_link(Type, Item, X, Y, Tile, Viewer) ->
    gen_server:start_link(?MODULE, [Type, Item, X, Y, Tile, Viewer], []).


get_type(Pid) ->
	gen_server:call(Pid,get_type).

get_state(Pid) ->
	gen_server:call(Pid,get_state).

picked_up(Pid) ->
	gen_server:call(Pid,picked_up).


%%%%%%=============================================================================
%%%%%% gen_server Callbacks
%%%%%%=============================================================================

init([Type, Item, X, Y, Tile, Viewer]) ->
    {ok, #state{id = self(), type = Type, x = X, y = Y, 
    			tile = Tile, viewer = Viewer}}.

handle_call(get_type, _From, #state{type = Type, item = Item} = State) ->
	{reply, {Type, Item},State};
handle_call(get_state, _From, State) ->
	{reply, State,State};
handle_call(picked_up, _From, State) ->
	{reply, ok, State};
handle_call(place, _From, #state{tile = Tile, x = X, y = Y, type = Type, item = Item} = State) ->
	tile:place_item(Tile, {self(),X,Y,Type,Item});
handle_call(Request, From, State) ->
    {stop, unexpected_call, {undefined, Request}, State}.

handle_cast(Request, State) ->
    {stop, unexpected_cast, State}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================