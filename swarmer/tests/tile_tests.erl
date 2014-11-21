-module(tile_tests).
-include_lib("eunit/include/eunit.hrl").
-import(tile,[start_link/0,stop_viewer/1,add_tile/2,update/2,get_tiles/1,get_population/1]).

add_data_test_() ->
    {setup,
    fun start/0,
    fun stop/1,
    fun add_data/1}.

start() ->
    {ok,Pid} = start_link(0,0,25),
    Pid.

add_data(Pid) ->
    tile:summon_entity(Pid,{Z1,{1,2}}),
    tile:summon_entity(Pid,{Z2,{5,6}}),
    tile:summon_entity(Pid,{Z3,{7,7}}).
    
test(Pid) ->
    [?_assert(tile:get_population(Pid) =:= [[1,1,2],[2,5,6],[3,7,7]])].

stop(Pid) -> 
    tile:stop_viewer(Pid).