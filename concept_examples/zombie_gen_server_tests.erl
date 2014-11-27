-module(zombie_gen_server_tests).
-include_lib("eunit/include/eunit.hrl").
-record(zombie,{name, colour=green, description}).

% ok it silly but you can run it in the shell with eunit:test(zombie_gen_server_test).

buy_zombie_test_() ->
    {setup,
    fun start/0,
    fun stop/1,
    fun buy/1}.
    
start() ->
    {ok,Pid} = zombie_gen_server:start_link(),
    Pid.

stop(Pid) -> 
    zombie_gen_server:close_shop(Pid).

buy(Pid) ->
    Z = zombie_gen_server:buy_zombie(Pid,"Test Zombie",brown,"A rotten corpse"),
    [?_assert(Z#zombie.name == "Test Zombie")].

%test_set_() ->
    %{foreach,
    %fun start_/0,
    %fun stop_/1,
    %[fun buy_/1]}.
    
