%%%-------------------------------------------------------------------
%%% @author Chris Yunker <chris@yunker.io>
%%% @copyright (C) 2014, Chris Yunker
%%% @doc Create and manipulate XMPP JID types
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xcl_jid).

-include("xcl.hrl").

-export([make/2, make/3,
         node/1,
         domain/1,
         resource/1,
         bare/1,
         to_binary/1,
         from_binary/1,
         bare_to_binary/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec make(binary() | string(), binary() | string()) -> xcl:jid().
make(Node, Domain) ->
    #jid{node = xcl_util:to_binary(Node),
         domain = xcl_util:to_binary(Domain)}.

-spec make(binary() | string(), binary() | string(), binary() | string()) ->
    xcl:jid().
make(Node, Domain, Resource) ->
    #jid{node = xcl_util:to_binary(Node),
         domain = xcl_util:to_binary(Domain),
         resource = xcl_util:to_binary(Resource)}.

-spec node(xcl:jid()) -> binary().
node(Jid) -> Jid#jid.node.

-spec domain(xcl:jid()) -> binary().
domain(Jid) -> Jid#jid.domain.

-spec resource(xcl:jid()) -> binary().
resource(Jid) -> Jid#jid.resource.

-spec bare(xcl:jid()) -> xcl:jid().
bare(Jid) ->
    Jid#jid{resource = <<>>}.

-spec to_binary(xcl:jid()) -> binary().
to_binary(#jid{node = Node, domain = Domain, resource = <<>>}) ->
    <<Node/binary, "@", Domain/binary>>;
to_binary(#jid{node = Node, domain = Domain, resource = Resource}) ->
    <<Node/binary, "@", Domain/binary, "/", Resource/binary>>.

-spec from_binary(binary()) -> xcl:jid().
from_binary(Jid) ->
    AtPos = case binary:match(Jid, <<"@">>, []) of
        nomatch -> throw({error, bad_jid});
        {AP, _} -> AP
    end,
    Node = binary:part(Jid, {0, AtPos}),
    case binary:match(Jid, <<"/">>, []) of
        nomatch ->
            Domain = binary:part(Jid, {AtPos + 1, size(Jid) - AtPos - 1}),
            #jid{node = Node,
                 domain = Domain};
        {SlashPos, _} ->
            Domain = binary:part(Jid, {AtPos + 1, SlashPos - AtPos - 1}),
            Resource = binary:part(Jid,
                                   {SlashPos + 1, size(Jid) - SlashPos - 1}),
            #jid{node = Node,
                 domain = Domain,
                 resource = Resource}
    end.

-spec bare_to_binary(xcl:jid()) -> binary().
bare_to_binary(Jid) ->
    to_binary(Jid#jid{resource = <<>>}).

