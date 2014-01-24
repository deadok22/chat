%%%-------------------------------------------------------------------
%%% @author deadok22
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2014 8:13 PM
%%%-------------------------------------------------------------------
-module(chatserver_utils).
-author("deadok22").

%% API
-export([get_timestamp/0]).

get_timestamp() ->
  {Mega, Sec, Micro} = erlang:now(),
  (Mega * 1000000 + Sec) * 1000000 + Micro.