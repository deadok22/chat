%%%-------------------------------------------------------------------
%%% @author deadok22
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2014 7:55 PM
%%%-------------------------------------------------------------------
-module(chatserver_protocol).
-author("deadok22").

%% API
-export([deserialize/1, serialize/1]).

%% deserialize a command and a command handler module for this command
-spec deserialize(Package :: iolist()) -> {Command :: term(), Module :: module()} | bad_package.
deserialize(Package) ->
  %%TODO implement me
  error(not_implemented).

serialize(Response) ->
  error(not_implemented).
