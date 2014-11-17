zombie-swarm
============

co600 zombie swarm project

Build Project
=============
Use the included rebar to build the application

  swarmer/rebar get-deps

  swarmer/rebar compile

Run Project
===========
From /zombie-swarm/swarmer;
Enter in bash:
  erl -pa deps/*/ebin/ -pa ebin/

Enter from Erlang Shell
  application:ensure_all_started(swarm).
  swarm_server:start().

Open file /zombie-swarmer/client/swarmclient.html

Documentation
=============

All the corpus type documentation is in latex files you can build them into a pdf using pdflatex document.tex or into html using htlatex document.tex

| table         | table table  |
| ------------- | ------------- |
| Content Cell  | Content Cell  |
| Content Cell  | Content Cell  |
