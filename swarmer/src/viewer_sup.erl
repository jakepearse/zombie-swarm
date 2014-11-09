-module(viewer_sup).
-author("Jake Pearse jp480@kent.ac.uk").

-behaviour(supervisor).
 
-export([start_link/1]).
-export([init/1]).
 
start_link([]) ->
supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
{ok,{{simple_one_for_one,1,60},
  [{normal_viewer, {viewer,start_link,[]},
    permanent,1000,worker,[viewer]}]}}.
