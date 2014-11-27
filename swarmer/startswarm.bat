erl -pa deps/cowboy/ebin -pa deps/cowlib/ebin -pa deps/jsx/ebin -pa deps/mochijson2/ebin -pa deps/ranch/ebin  -pa ebin/ -eval 'application:ensure_all_started(swarm)' -noshell
