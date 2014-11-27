-module(zombie_gen_server).

% gen_server requirements
-behaviour(gen_server).

-export([code_change/3,handle_call/3,handle_cast/2,
handle_info/2,init/1,terminate/2]).

% my stuff

-export([start_link/0, buy_zombie/4, return_zombie/2, close_shop/1]).

-record(zombie,{name, colour=green, description}).

% now some actual functions
% it ripped off of learnyousome but now its a zombie shop

start_link() -> gen_server:start_link(?MODULE, [], []).

buy_zombie(Pid, Name, Color, Description) ->
    gen_server:call(Pid, {order, Name, Color, Description}).

return_zombie(Pid, Zombie = #zombie{}) ->
    gen_server:cast(Pid, {return, Zombie}).

close_shop(Pid) ->
    gen_server:call(Pid, terminate).

init([]) -> 
    {ok,[]}. % aint nothin to see here

handle_call({order, Name, Colour, Description},_From, Zombies) ->
    if Zombies =:= [] ->
        {reply, make_zombie(Name, Colour, Description), Zombies};
    Zombies =/= [] -> % got to empty the stock
        {reply, hd(Zombies), tl(Zombies)}
    end;
    
handle_call(terminate, _From, Zombies) ->
    {stop, normal, ok, Zombies}.
    
handle_cast({return, Zombie = #zombie{}}, Zombies) ->
    {noreply, [Zombie|Zombies]}.

% this thing is for unexpected messages, like ones sent with !
handle_info(Msg, Zombies) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply,Zombies}.

terminate(normal, Zombies) ->
    [io:format("~p escaped in the wild.~n",[Z#zombie.name]) || Z <- Zombies],
    ok.
    
code_change(_OldVsn, State,_Extra) ->
    {ok,State}.

make_zombie(Name, Col, Desc) ->
    #zombie{name=Name, colour=Col, description=Desc}.
