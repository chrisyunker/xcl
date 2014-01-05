%%%-------------------------------------------------------------------
%%% @author Chris Yunker <chris@yunker.io>
%%% @copyright (C) 2014, Chris Yunker
%%% @doc Define and export XCL types
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xcl).

-include_lib("exml/include/exml_stream.hrl").
-include("xcl.hrl").

-type session() :: #session{}.
-type jid() :: #jid{}.
-type xmlel() :: #xmlel{}.
-type stanza() :: xmlstreamelement().

-export_type([session/0, jid/0, xmlel/0, stanza/0]).
