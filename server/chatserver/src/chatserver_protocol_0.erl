%%%-------------------------------------------------------------------
%%% @author deadok22
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jan 2014 8:11 PM
%%%-------------------------------------------------------------------
-module(chatserver_protocol_0).
-author("deadok22").

%% API
-export([deserialize/1, serialize/1]).

-include("chatserver_protocol_0.hrl").
-include("chat_message.hrl").

deserialize(<<?LOGIN:8, _:8, NameLength:32, UserName:NameLength/binary>>) ->
  {login, binary_to_list(UserName)};
deserialize(<<?FETCH:8, _:8, MessageId:64>>) ->
  {fetch, MessageId};
deserialize(<<?SEND:8, _:8, TextLength:32, Text:TextLength/binary>>) ->
  {send, binary_to_list(Text)};
deserialize(<<?USERLIST:8, _:8>>) ->
  userlist;
deserialize(<<?LOGOUT:8, _:8>>) ->
  logout;
deserialize(Packet) ->
  io:format("Ill formed packet passed to ~p: ~p~n", [?MODULE, Packet]),
  bad_packet.

serialize(Response) ->
  Packet = serialize_internal(Response),
  <<?VERSION:8, Packet/binary>>.

serialize_internal({?LOGIN_RESPONSE_OK, MessageId}) ->
  <<?LOGIN_RESPONSE_OK:8, 0:8, MessageId:64>>;
serialize_internal(?LOGIN_ERR) ->
  <<?LOGIN_ERR:8, 0:8>>;
serialize_internal({?MESSAGES_LIST, Messages}) ->
  MessagesData =
    <<<<Id:64, Timestamp:64, (serialize_text(Author))/binary, (serialize_text(Text))/binary>> ||
      #chat_message{id = Id, timestamp = Timestamp, author = Author, text = Text} <- Messages>>,
  <<?MESSAGES_LIST:8, 0:8, (length(Messages)):32, MessagesData/binary>>;
serialize_internal({?USER_LIST_RESPONSE, UserList}) ->
  <<?USER_LIST_RESPONSE:8, 0:8, (length(UserList)):32, <<<<(serialize_text(User))/binary>> || User <- UserList>>/binary>>;
serialize_internal({?SEND_MESSAGE_RESPONSE, MessageId}) ->
  <<?SEND_MESSAGE_RESPONSE, 0:8, MessageId:64>>;
serialize_internal(?LOGOUT_RESPONSE) ->
  <<?LOGOUT_RESPONSE:8, 0:8>>;
serialize_internal(Response) ->
  error({"Bad response.", Response}).

serialize_text(Text) ->
  TextBinary = list_to_binary(Text),
  TextBinaryLength = byte_size(TextBinary),
  if
    TextBinaryLength < 0 -> io:format("WTF!?!?!?!?!?!!?!?");
    true -> ok
  end,
  <<TextBinaryLength:32, TextBinary/binary>>.