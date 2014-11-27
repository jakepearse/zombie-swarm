-module(viewer_tests).
-include_lib("eunit/include/eunit.hrl").
%-import(viewer,[start_link/0,terminate/2,add_tile/2,update/2,get_tiles/1,get_population/1]).

add_data_test_() ->
    {setup,
    fun start/0,
    fun stop/1,
    fun add_data/1}.

start() ->
    {ok,VS} = veiwer_sup:start_link([]),
    {ok,TS}= tile:sup:start_link([]),
    {ok,Pid}= supervisor:start_child(VS,[]).
    
stop(Pid) -> 
    supervisor:terminate_child(VS,Pid).

add_data(Pid) ->
    {ok,Tile1} = supervisor:start_child(TS,[0,0,25]),
    {ok,Tile2 = supervisor:start_child(TS,[26,26,25]),
    viewer:add_tile(Tile1),
    viewer:add_tile(Tile2),
    ?_assert(viewer:get_tiles(V)=:=supervisor:which_children(TS)),
    ?_assert(dict:is_key(Tile1,viewer:get_population(Pid))),
    ?_assert(dict:fetch(Tile1,viewer:get_population(Pid))==[]),
    ?_assert(dict:fetch(data2,viewer:get_population(Pid))==[])
    ].
