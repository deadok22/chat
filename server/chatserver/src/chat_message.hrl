%%%-------------------------------------------------------------------
%%% @author deadok22
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2014 8:46 PM
%%%-------------------------------------------------------------------
-author("deadok22").


-record(chat_message, {
  id :: pos_integer(),
  timestamp :: pos_integer(),
  author :: nonempty_string(),
  text :: nonempty_string()
}).