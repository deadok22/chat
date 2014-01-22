%%%-------------------------------------------------------------------
%%% @author deadok22
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2014 6:58 PM
%%%-------------------------------------------------------------------
-module(chatserver_supervisor).
-author("deadok22").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  Children = acceptors_specs(),
  {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
acceptors_specs() ->
  Socket = chatserver_acceptor:open_server_socket(),
  Restart = permanent,
  Shutdown = 2000,
  Type = worker,
  [{"acceptor_" ++ integer_to_list(Id),
    {chatserver_acceptor, start_link, [Socket]},
    Restart,
    Shutdown,
    Type,
    [chatserver_acceptor]
  } || Id <- lists:seq(0, acceptors_count())].

acceptors_count() ->
  case application:get_env(acceptors_count) of
    undefined ->
      throw("Acceptors count is not specified");
    AcceptorsCount -> AcceptorsCount
  end.