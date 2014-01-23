%%%-------------------------------------------------------------------
%%% @author deadok22
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2014 9:00 PM
%%%-------------------------------------------------------------------
-module(chatserver_client_worker).
-author("deadok22").

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-include("chatserver_client_worker_state.hrl").

%%%===================================================================
%%% API
%%%===================================================================

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
init([Socket]) ->
  io:format("Client connected: ~p~n", [Socket]),
  {ok, #state{socket = Socket}}.

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
handle_call(Unexpected, _From, State) ->
  {stop, {unexpected_call, Unexpected}, State}.

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
handle_cast(init, State) ->
  set_opts(State);
handle_cast(Unexpected, State) ->
  {stop, {unexpected_cast, Unexpected}, State}.

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
handle_info({tcp, _Socket, Packet}, State) ->
  packet_received(Packet, State);
handle_info({tcp_closed, _Socket}, State) ->
  {stop, tcp_closed, State};
handle_info(Unexpected, State) ->
  {stop, {unexpected_info, Unexpected}, State}.

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
terminate(_Reason, #state{socket = Socket}) ->
  io:format("Client disconnected: ~p~n", [Socket]),
  gen_server:call(chatserver_client_registry, {unregister_client, self()}),
  gen_tcp:close(Socket),
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
packet_received(Packet, State) ->
  io:format("Got a packet from ~p~n", [State#state.socket]),
  case chatserver_protocol:deserialize(Packet) of
    {Command, Module} ->
      execute_command(Command, Module, State);
    bad_packet ->
      {stop, bad_packet, State}
  end.

set_opts(#state{socket = Socket} = State) ->
  Opts = [
    {active, true},
    {packet, 4},
    binary
  ],
  case inet:setopts(Socket, Opts) of
    {error, What} ->
      {stop, {"inet:setopts failed", What}, State};
    ok ->
      {noreply, State}
  end.

execute_command(Command, Module, #state{socket = Socket} = State) ->
  case Module:execute_command(self(), Command, State) of
    {ok, Response, NewState} ->
      gen_tcp:send(Socket, chatserver_protocol:serialize(Response)),
      {noreply, NewState};
    Error ->
      {stop, Error, State}
  end.