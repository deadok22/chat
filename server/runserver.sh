#!/bin/sh

cp ./chatserver/src/chatserver.app ./ebin/

erl -pa ./ebin/ -s chatserver_launcher main -noshell