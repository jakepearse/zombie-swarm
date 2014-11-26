#!/bin/bash
erl -pa deps/*/ebin -pa ebin/ -eval 'application:ensure_all_started(swarm)' -noshell
