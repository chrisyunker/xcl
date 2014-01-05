%%%-------------------------------------------------------------------
%%% @author Chris Yunker <chris@yunker.io>
%%% @copyright (C) 2014, Chris Yunker
%%% @doc Utility functions
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xcl_util).

-include("xcl.hrl").

-export([id/0, id/1,
         to_binary/1,
         to_list/1,
         to_integer/1]).

-define(MAX_ID, 2147483648).

%%%===================================================================
%%% API
%%%===================================================================

-spec id() -> binary().
id() ->
    id(<<"xcl">>).

-spec id(binary()) -> binary().
id(Prefix) ->
    <<Prefix/binary, "_",
      (integer_to_binary(random:uniform(?MAX_ID)))/binary>>.

-spec to_binary(binary() | string()) -> binary().
to_binary(Bin) when is_binary(Bin) ->
    Bin;
to_binary(List) when is_list(List) ->
    erlang:list_to_binary(List).

-spec to_list(string() | binary()) -> string().
to_list(List) when is_list(List) ->
    List;
to_list(Bin) when is_binary(Bin) ->
    erlang:binary_to_list(Bin).

-spec to_integer(integer() | binary() | string()) -> integer().
to_integer(Int) when is_integer(Int) ->
    Int;
to_integer(List) when is_list(List) ->
    erlang:list_to_integer(List);
to_integer(Bin) when is_binary(Bin) ->
    erlang:binary_to_integer(Bin).

