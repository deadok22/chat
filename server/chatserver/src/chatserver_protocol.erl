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
-spec deserialize(Packet :: iolist()) -> {Command :: term(), Module :: module()} | bad_package.
deserialize(Packet) ->
  case strip_protocol_version(Packet) of
    {Version, VersionStrippedPacket} ->
      case get_modules(Version) of
        unknown_version -> bad_packet;
        {SerializerModule, CommandExecutorModule} ->
          case SerializerModule:deserialize(VersionStrippedPacket) of
            bad_packet ->
              bad_packet;
            Command ->
              {Command, CommandExecutorModule}
          end
      end;
    bad_packet ->
      bad_packet
  end.

serialize({Version, Response}) ->
  Serializer = serializer_module(Version),
  Serializer:serialize(Response).

strip_protocol_version(<<Version:8, Packet/binary>>) ->
  {Version, Packet};
strip_protocol_version(_Packet) ->
  bad_packet.

get_modules(Version) ->
  try {serializer_module(Version), command_executor_module(Version)} of
    {S, E} -> {S, E}
  catch
    throw:{error, module_not_found} -> unknown_version
  end.

serializer_module(Version) ->
  get_module(list_to_atom("chatserver_protocol_" ++ integer_to_list(Version))).

command_executor_module(Version) ->
  get_module(list_to_atom("chatserver_commands_executor_" ++ integer_to_list(Version))).

get_module(Module) ->
  case code:ensure_loaded(Module) of
    {module, _} -> Module;
    {error, _What} -> throw({error, module_not_found})
  end.