%%%-------------------------------------------------------------------
%%% @author Chris Yunker <chris@yunker.io>
%%% @copyright (C) 2014, Chris Yunker
%%% @doc Logging functions
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xcl_log).

-export([debug/1, debug/2,
         info/1, info/2,
         warning/1, warning/2,
         error/1, error/2]).

%%%===================================================================
%%% API
%%%===================================================================

debug(Message) ->
    debug(Message, []).
debug(Message, Args) ->
    lager:debug(Message, Args).

info(Message) ->
    info(Message, []).
info(Message, Args) ->
    lager:info(Message, Args).

warning(Message) ->
    warning(Message, []).
warning(Message, Args) ->
    lager:warning(Message, Args).

error(Message) ->
    ?MODULE:error(Message, []).
error(Message, Args) ->
    lager:error(Message, Args).


%%%===================================================================
%%% Private functions
%%%===================================================================

