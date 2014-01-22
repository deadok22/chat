%%%-------------------------------------------------------------------
%%% @author deadok22
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2014 7:38 PM
%%%-------------------------------------------------------------------
-module(chatserver_client_worker).
-author("deadok22").

%% API
-export([run/0]).

run() ->
  Socket = get_client_socket(),
  worker_loop(Socket).

worker_loop(Socket) ->
  receive
    {tcp, Socket, Package} ->
      package_received(Socket, Package);
    Unexpected ->
      unexpected_message(Unexpected)
  end,
  worker_loop(Socket).

package_received(Socket, Package) ->
  %%TODO implement me
  error(not_implemented).

get_client_socket() ->
  receive
    {client_socket, Socket} ->
      set_opts(Socket);
    Unexpected ->
      unexpected_message(Unexpected)
  end.

set_opts(Socket) ->
  %% TODO configure packaging
  case inet:setopts(Socket, [{active, true}]) of
    {error, What} ->
      throw({"inet:setopts failed", What});
    ok -> ok
  end.

unexpected_message(Message) ->
  throw({"Unexpected message received", Message}).