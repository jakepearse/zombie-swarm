-module(swarm_sup).
-author("").

-behaviour(supervisor).
 
-export([start_link/1]).

-export([init/1]).
 
start_link([]) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
	{ok,{{one_for_one,1,60},
  		[{enviroment, {enviroment,start_link,[]},
    	permanent,1000,worker,[enviroment]}]}}.
