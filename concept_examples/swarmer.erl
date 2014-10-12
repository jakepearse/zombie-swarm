-module(swarmer).
-export([start/0]).

start() ->
    spawn(fun () -> {ok, Sock} = gen_tcp:listen(8000, [{active, false}]), 
                    loop(Sock) end).

loop(Sock) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    Handler = spawn(fun () -> handle(Conn) end),
    gen_tcp:controlling_process(Conn, Handler),
    loop(Sock).

handle(Conn) ->
    gen_tcp:send(Conn, response(nums())),
    gen_tcp:close(Conn);

handle(Conn,X) ->
    gen_tcp:send(Conn, response(nums(X))),
    gen_tcp:close(Conn).


response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
      io_lib:fwrite(
         "HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
         [size(B), B])).

nums() -> nums({random:uniform(50),random:uniform(50)});
nums(X) ->
{A,B} = X -> io:format("~d,~d",[A,B]).
