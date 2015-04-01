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

The application will need to be compiled for your system, so you need to copy
both corpus/client/ and corpus/swarmer/ to a local directory before continuing
with the setup instructions.

There is a video in /corpus/ext/ showing the system running, and an explanation
of how it works.

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
2)  Change direcory to local_path_to_corpus\ext\rebar\
3)  Run bootstrap.bat
4)  Copy the compiled rebar application to local_path_to_corpus\swarmer\
5)  Run 'rebar co'

================================================================================
Setup - Linux/Unix/OSX
================================================================================
1) Set up the rebar application for your system.

    1a)  Run the 'bootstrap' from local_path_to_corpus/ext/rebar/
        This will compile rebar for your system

    1b)  Copy the compiled rebar application to local_path_to_corpus/swarmer/

2) Use rebar to compile the application and dependencies.

    2a)  In local_path_to_corpus/swarmer/, run './rebar co'

================================================================================
Running the Simulation
================================================================================
1)  Run 'swarmer'

      In Windows         -  run swarmer.bat from local_path_to_corpus\swarmer\

      Unix like systems  -  run swarmer from local_path_to_corpus/swarmer/
      
2)  Open client.html from local_path_to_corpus/client/

================================================================================
Troubleshooting
================================================================================
No simulation in browser:

    If the rowser is only displayng the form but no iimage, it cannot connet to
    the swarmer application, ensure you have started
    local_path_to_corpus/swarmer/swarmer and it is resting at erlang prompt
    before refreshing the page.

    If you are using windows, ensure you are able to serve WebSocket connections
    over port 8080

Dependencies

    All the dependencies are included in the swarmer/deps folder if you have
    issues - rebar can resolve dependencies using git, assuming git is installed
    'rebar get-deps' will download and compile all the required libraries.
