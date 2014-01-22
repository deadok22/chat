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
      packet_received(Socket, Package);
    {tcp_closed, Socket} ->
      %%TODO notify about client's disconnection
      stop_worker(Socket, ok);
    Unexpected ->
      unexpected_message(Unexpected)
  end,
  worker_loop(Socket).

packet_received(Socket, Packet) ->
  case chatserver_protocol:deserialize(Packet) of
    {Command, Module} ->
      Response = Module:execute_command(Command),
      send_response(Socket, Response);
    bad_package ->
      stop_worker(Socket, bad_package)
  end.

send_response(Socket, Response) ->
  case chatserver_protocol:serialize(Response) of
    bad_response ->
      stop_worker(Socket, bad_response);
    ResponsePacket ->
      gen_tcp:send(Socket, ResponsePacket)
  end.


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

stop_worker(Socket, What) ->
  gen_tcp:close(Socket),
  do_stop(What).

do_stop(ok) ->
  exit(self(), ok);
do_stop(What) ->
  throw(What).