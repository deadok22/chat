%%%-------------------------------------------------------------------
%%% @author deadok22
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jan 2014 9:17 PM
%%%-------------------------------------------------------------------
-author("deadok22").

-define(VERSION, 0).

%% command codes
-define(LOGIN, 1).
-define(FETCH, 2).
-define(SEND, 3).
-define(USERLIST, 4).
-define(LOGOUT, 5).
-define(LOGIN_RESPONSE_OK, 6).
-define(LOGIN_ERR, 7).
-define(MESSAGES_LIST, 8).
-define(USER_LIST_RESPONSE, 9).
-define(SEND_MESSAGE_RESPONSE, 10).
-define(LOGOUT_RESPONSE, 11).