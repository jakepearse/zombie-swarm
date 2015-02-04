-module(tile_tests).
-include_lib("eunit/include/eunit.hrl").

tile_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [{"add test data",fun test_population/0}%,
        % {"update test data", fun test_update/0}
        ]
    }.

setup() ->
    {ok,Pid} = tile:start_link(test_tile,0,0,25).
    % erlang:register(test_tile,Pid).

teardown(_Thing) -> 
    error_logger:error_report(_Thing),
    tile:terminate(test_tile).

test_population() ->
    {ok,V} = viewer:start_link(),
    {ok,Z1} = zombie_fsm:start_link(1,2,test_tile,25,1,1,V,1,1),
    {ok,Z2} = zombie_fsm:start_link(5,6,test_tile,25,1,1,V,1,1),
    {ok,Z3} = zombie_fsm:start_link(7,7,test_tile,25,1,1,V,1,1),
    tile:summon_entity(test_tile,{Z1,{1,2}}),
    tile:summon_entity(test_tile,{Z2,{5,6}}),
    tile:summon_entity(test_tile,{Z3,{7,7}}),
    {Zombies,Humans,_,_,_,_,_,_,_} = tile:get_state(test_tile),
    ?assertEqual(maps:size(Zombies), 3),
    ?assertEqual(maps:find(Z1), {ok, {zombie,{1,2}}}).

% test_update() ->
%     {ok,V} = viewer:start_link(),
%     {ok,Z1} = zombie_fsm:start_link(1,2,test_tile,V),
%     tile:summon_entity(test_tile,{Z1,{1,2}}),
%     tile:update_entity(test_tile,{Z1,{1,2}},{5,5},n,0),
%     ?assertEqual(tile:get_population(test_tile), [[0,5,5]]).

    

