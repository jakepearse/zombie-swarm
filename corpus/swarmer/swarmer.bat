erl -pa deps/cowboy/ebin -pa deps/cowlib/ebin -pa deps/jsx/ebin -pa deps/ranch/ebin  -pa ebin/ -s swarm_server -name swarmer@127.0.0.1 -setcookie swarmer
