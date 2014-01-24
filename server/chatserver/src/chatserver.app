%%%-------------------------------------------------------------------
%%% @author deadok22
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2014 6:57 PM
%%%-------------------------------------------------------------------
{application, chatserver, [
  {description, "A chat server"},
  {vsn, "1"},
  {registered, []},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {chatserver, []}},
  {env, [
    {port, 55896},
    {acceptors_count, 10},
    {message_history_size, 100},
    {statistics_report_delay, 5000}
  ]}
]}.