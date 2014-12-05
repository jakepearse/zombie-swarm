-module(swarm_sup).
-author("").

-behaviour(supervisor).
 
-export([start_link/1]).

-export([init/1]).
 
start_link([]) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
    Environment = {enviroment, {enviroment,start_link,[]},
                    permanent,1000,worker,[enviroment]},
    TileSup = {tile_sup, {tile_sup, start_link, []},
                    permanent,infinity,supervisor,[tile_sup]},
    ViewerSup = {viewer_sup, {viewer_sup, start_link, []},
                    permanent,infinity,supervisor,[viewer_sup]},
    ZombieSup = {zombie_sup, {zombie_sup, start_link, []},
                    permanent,infinity,supervisor,[zombie_sup]},
	{ok,{{one_for_one,1,60},[Environment,TileSup, ViewerSup, ZombieSup]}}.
