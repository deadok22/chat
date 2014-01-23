%%%-------------------------------------------------------------------
%%% @author deadok22
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jan 2014 9:53 PM
%%%-------------------------------------------------------------------
-module(chatserver_launcher).
-author("deadok22").

%% API
-export([main/0]).

main() ->
  application:start(chatserver).