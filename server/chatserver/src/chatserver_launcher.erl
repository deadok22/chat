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
%% Uncomment to run profiler
%%   eprof:start(),
%%   eprof:profile(lists:delete(eprof, registered()), fun() -> run_chatserver(30000) end),
%%   eprof:log("profiling.txt"),
%%   eprof:analyze()
  run_chatserver(300000).


run_chatserver(For) ->
  case application:start(chatserver) of
    ok -> timer:sleep(For), application:stop(chatserver);
    {error, Reason} -> exit(Reason)
  end.