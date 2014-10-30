-module(viewer_tests).
-include_lib("eunit/include/eunit.hrl").
-import(viewer,[start_link/1,stop_viewer/1,add_tile/2,update/2,get_tiles/1,get_population/1]).

add_data_test_() ->
    {setup,
    fun start/0,
    fun stop/1,
    fun add_data/1}.

start() ->
    {ok,Pid} = viewer:start_link(),
    Pid.
    
stop(Pid) -> 
    viewer:stop_viewer(Pid).

add_data(Pid) ->
    Data1 = {data1,[a,b,c]},
    Data2 = {data2,[x,y,z]},
    viewer:add_tile(Pid,Data1),
    viewer:add_tile(Pid,Data2),
    [?_assert(viewer:get_tiles(Pid)==[data1,data2]),
    ?_assert(dict:is_key(data1,viewer:get_population(Pid))),
    ?_assert(dict:fetch(data1,viewer:get_population(Pid))==[a,b,c]),
    ?_assert(dict:fetch(data2,viewer:get_population(Pid))==[x,y,z])
    ].
