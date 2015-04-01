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
Linux/Unix

  From /zombie-swarm/swarmer

    ./swarmer

Windows

  From \zombie-swarm\swarmer

    .\startswarm.bat

Open file /zombie-swarmer/client/newclient.html

Documentation
=============

All the corpus type documentation is in latex files you can build them into a pdf using pdflatex document.tex or into html using htlatex document.tex

| table         | table table  |
| ------------- | ------------- |
| Content Cell  | Content Cell  |
| Content Cell  | Content Cell  |

================================================================================
Swarmer
================================================================================
System Requirements
  Erlang OTP 17.4-Downloadable for all systems here
                  https://www.erlang-solutions.com/downloads/download-erlang-otp
  A browser      -We recommend using Mozilla Firefox or Google Chrome

The application is compatible with any operating system that can run the Erlang
OTP. We recommend running on a Linux/Unix based system as setting up an Erlang
environment on Windows can be awkward.

================================================================================
Setup - Windows
================================================================================
In Windows, installing Erlang/OTP isn't enough to run an Erlang application. You
will need to set up an 'environment variable' and 'path' to allow Erlang to run
elsewhere in the system.
Though old, this guide is still relevant for Windows 7 based computers:
  https://code.google.com/p/erlang-quiz/wiki/InstallingErlangWindows

This will need to be completed before setting up the rebar application.

Our application also uses 'localhost' at 127.0.0.1, this will need to be added
to the Windows 'hosts' file.

Now you will be ready to set up the application.
1)  Open cmd.exe
2)  Change direcory to \corpus\ext\rebar\
3)  Run bootstrap.bat
4)  Copy the compiled rebar application to corpus\swarmer\
5)  Run 'rebar co'

================================================================================
Setup - Linux/Unix/OSX
================================================================================
Firstly, set up the rebar application for your system.

1)  Run the 'bootstrap' from corpus/ext/rebar/
      This will compile rebar for your system
2)  Copy the compiled rebar application to corpus/swarmer/

Now, use rebar to compile the application and dependencies.

3)  In corpus/swarmer/, run './rebar co'

================================================================================
Running the Simulation
================================================================================
1)  Run 'swarmer'
      In Windows         -  run swarmer.bat from corpus\swarmer\
      Unix like systems  -  run swarmer from corpus/swarmer/
2)  Open client.html from corpus/client/
