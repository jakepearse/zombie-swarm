-module(enviroment_tests).
-include_lib("eunit/include/eunit.hrl").
%-import(viewer,[start_link/0,stop_viewer/1,add_tile/2,update/2,get_tiles/1,get_population/1]).

add_data_test_() ->
    {setup,
    fun start/0,

    fun add_data/1}.

start() ->
    {ok,E} = enviroment:start_link(),
    E.
    


add_data(E) ->
    ok = enviroment:make_grid(E,3,3,25),
    ok = enviroment:set_swarm(E,5),
    [?_assert(lists:all(fun(X)->is_pid(X) end,enviroment:get_grid(E))),
    ?_assert(length(enviroment:get_grid(E))=:=9),
    ?_assert(length(enviroment:report(E))=:=5),
    ?_assert(lists:all(fun(Y)->is_list(Y) end,enviroment:report(E))),
    %?_assert(dict:fetch(data1,viewer:get_population(Pid))==[a,b,c]),
    %?_assert(dict:fetch(data2,viewer:get_population(Pid))==[x,y,z])
    ].
