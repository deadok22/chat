%%%-------------------------------------------------------------------
%%% @author deadok22
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Nov 2013 6:59 PM
%%%-------------------------------------------------------------------
-module(chatserver_messages).
-author("deadok22").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  messages = [] :: list(),
  message_history_max :: non_neg_integer(),
  messages_count = 0 :: non_neg_integer(),
  new_message_id = 1:: non_neg_integer()
}).

-include("chat_message.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(MessageHistorySize :: non_neg_integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(MessageHistorySize) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [MessageHistorySize], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([MessageHistorySize]) ->
  {ok, #state{message_history_max = MessageHistorySize}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({post_message, MessagePrototype}, _From, State) ->
  NewMessage = MessagePrototype#chat_message{id = State#state.new_message_id, timestamp = chatserver_utils:get_timestamp()},
  NewState = State#state{
    messages = [NewMessage | State#state.messages],
    messages_count = State#state.messages_count + 1,
    new_message_id = State#state.new_message_id + 1
  },
%%   io:format("<~p>[~p]~s: ~s~n", [NewMessage#chat_message.id, NewMessage#chat_message.timestamp, NewMessage#chat_message.author, NewMessage#chat_message.text]),
  {reply, NewMessage, drop_extra_messages(NewState)};
handle_call(get_last_message_id, _From, #state{messages = []} = State) ->
  {reply, 0, State};
handle_call(get_last_message_id, _From, #state{messages = [#chat_message{id = Id} | _]} = State) ->
  {reply, Id, State};
handle_call({get_messages, FirstKnownId}, _From, #state{messages = Messages} = State) ->
  ReversedMessages = lists:takewhile(fun(#chat_message{id = Id}) -> Id /= FirstKnownId end, Messages),
  {reply, lists:reverse(ReversedMessages), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%TODO re-implement messages storage - this impl is slow.
drop_extra_messages(#state{
    message_history_max = MessagesHistoryMax,
    messages_count = MessagesCount,
    messages = Messages
} = State) ->
  case MessagesHistoryMax + 1 of
    MessagesCount ->
      State#state{
        messages = lists:sublist(Messages, MessagesHistoryMax),
        messages_count = MessagesHistoryMax};
    _ -> State
  end.