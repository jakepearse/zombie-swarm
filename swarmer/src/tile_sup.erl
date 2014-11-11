-module(tile_sup).
-author("Joe Mitchard jm710@kent.ac.uk").

-behaviour(supervisor).
 
-export([start_link/1]).

-export([init/1]).
 
start_link([]) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
	{ok,{{simple_one_for_one,1,60},
  		[{normal_tile, {tile,start_link,[]},
    	permanent,1000,worker,[tile]}]}}.

%start_child(?MODULE,[]) ->
%	supervisor:start_child({local,?MODULE}, ?MODULE, [X,Y,Size]).