%%%-------------------------------------------------------------------
%%% @author deadok22
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2014 8:11 PM
%%%-------------------------------------------------------------------
-author("deadok22").

-record(command_stats, {command :: atom(), time_avg :: float(), count :: pos_integer()}).