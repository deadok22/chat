%%%-------------------------------------------------------------------
%%% @author deadok22
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jan 2014 8:27 PM
%%%-------------------------------------------------------------------
-module(chatserver_commands_executor_0).
-author("deadok22").

%% API
-export([execute_command/3]).

-include("chat_message.hrl").

-include("chatserver_protocol_0.hrl").

execute_command(WorkerPid, {login, UserName}, State) ->
  case gen_server:call(chatserver_client_registry, {register_client, WorkerPid, UserName}) of
    already_registered ->
      response(?LOGIN_ERR, State);
    ok ->
      response({?LOGIN_RESPONSE_OK, get_last_message_id()}, State)
  end;
execute_command(_WorkerPid, {fetch, MessageId}, State) ->
  response({?MESSAGES_LIST, gen_server:call(chatserver_messages, {get_messages, MessageId})}, State);
execute_command(WorkerPid, {send, Text}, State) ->
  case gen_server:call(chatserver_client_registry, {is_logged_in, WorkerPid}) of
    false ->
      response(?LOGIN_ERR, State);
    {true, Name} ->
      NewMessage = gen_server:call(chatserver_messages, {post_message, #chat_message{author = Name, text = Text}}),
      response({?SEND_MESSAGE_RESPONSE, NewMessage#chat_message.id}, State)
  end;
execute_command(_WorkerPid, userlist, State) ->
  {ok, {?USER_LIST_RESPONSE, gen_server:call(chatserver_client_registry, get)}, State};
execute_command(WorkerPid, logout, State) ->
  gen_server:call(chatserver_client_registry, {unregister_client, WorkerPid}),
  response(?LOGOUT_RESPONSE, State);
execute_command(_WorkerPid, Command, _State) ->
  {unknown_command, Command}.


response(What, State) ->
  {ok, {?VERSION, What}, State}.

get_last_message_id() ->
  gen_server:call(chatserver_messages, get_last_message_id).