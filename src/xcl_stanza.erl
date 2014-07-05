%%%-------------------------------------------------------------------
%%% @author Chris Yunker <chris@yunker.io>
%%% @copyright (C) 2014, Chris Yunker
%%% @doc Create and extract XMPP stanzas
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xcl_stanza).

-include_lib("exml/include/exml_stream.hrl").
-include("xcl.hrl").

-export([attr/2,
         el/3,
         el_simple/2,
         el_xmlns/1, el_xmlns/2,
         set_attr/2,
         add_subel/2,
         get_name/1,
         get_id/1,
         get_type/1,
         get_sender/1,
         get_recipient/1,
         get_xmlns/1,
         get_error/1]).

-export([iq/3,
         iq_get/3,
         iq_set/3,
         presence/1,
         presence_roster/3,
         message/4,
         get_message_body/1,
         get_message_subject/1]).

-export([roster_req_list/1,
         roster_set_item/2,
         roster_set_items/2,
         roster_remove_item/2,
         roster_remove_items/2,
         get_roster_list/1]).

-export([privacy_req_list_names/1,
         privacy_req_lists/2,
         privacy_set_list/3,
         privacy_set_default/2,
         privacy_unset_default/1,
         privacy_set_active/2,
         privacy_unset_active/1,
         get_privacy_list_names/1,
         get_privacy_list/1]).

-export([muc_join/2,
         muc_leave/2,
         muc_disco_info/2,
         muc_disco_items/2,
         get_muc_count/1,
         get_muc_status_codes/1]).

-export([stream_start/2,
         stream_end/0,
         ws_open/1,
         ws_close/0,
         starttls/0,
         compress/1,
         auth/2,
         auth_plain/2,
         bind/1,
         session/0,
         get_stream_features/1,
         verify_starttls/1,
         verify_compress/1,
         verify_auth/1,
         verify_bind/1]).

-import(xcl_util, [to_binary/1,
                   id/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%% Core functions
%%%-------------------------------------------------------------------

-spec attr(binary() | string(), binary() | string()) -> xmlattr().
attr(Name, Value) ->
    {to_binary(Name), exml:escape_attr(to_binary(Value))}.

-spec el(binary() | string(), [xmlattr()], [#xmlel{} | #xmlcdata{}]) -> #xmlel{}.
el(Name, Attrs, Children) ->
    #xmlel{name = to_binary(Name),
           attrs = Attrs,
           children = Children}.

-spec el_simple(binary() | string(), iodata()) -> #xmlel{}.
el_simple(Name, Content) ->
    el(Name, [], [exml:escape_cdata(Content)]).

-spec el_xmlns(binary() | string()) -> #xmlel{}.
el_xmlns(Xmlns) ->
    el(<<"x">>, [attr(<<"xmlns">>, Xmlns)], []).

-spec el_xmlns(binary() | string(), [#xmlel{} | #xmlcdata{}]) -> #xmlel{}.
el_xmlns(Xmlns, Children) ->
    el(<<"x">>, [attr(<<"xmlns">>, Xmlns)], Children).

-spec set_attr(#xmlel{}, xmlattr()) -> #xmlel{}.
set_attr(El, Attr) ->
    El#xmlel{attrs = lists:keystore(element(1, Attr), 1, El#xmlel.attrs, Attr)}.

-spec add_subel(#xmlel{}, #xmlel{} | [#xmlel{}]) -> #xmlel{}.
add_subel(El, SubEls) when is_list(SubEls) ->
    El#xmlel{children = SubEls ++ El#xmlel.children};
add_subel(El, SubEl) ->
    add_subel(El, [SubEl]).

-spec get_name(#xmlel{}) -> binary().
get_name(#xmlel{name = Name}) ->
    Name.

-spec get_id(#xmlel{}) -> binary() | undefined.
get_id(El) ->
    exml_query:attr(El, <<"id">>, undefined).

-spec get_type(#xmlel{}) -> binary() | undefined.
get_type(El) ->
    exml_query:attr(El, <<"type">>, undefined).

-spec get_sender(#xmlel{}) -> binary() | undefined.
get_sender(El) ->
    exml_query:attr(El, <<"from">>, undefined).

-spec get_recipient(#xmlel{}) -> binary() | undefined.
get_recipient(El) ->
    exml_query:attr(El, <<"to">>, undefined).

-spec get_xmlns(#xmlel{}) -> binary() | undefined.
get_xmlns(El) ->
    case exml_query:subelement(El, <<"x">>, undefined) of
        undefined ->
            undefined;
        XEl ->
            exml_query:attr(XEl, <<"xmlns">>, undefined)
    end.

-spec get_error(#xmlel{}) -> {binary(), binary(), [#xmlel{}]} | undefined.
get_error(El) ->
    case exml_query:subelement(El, <<"error">>, undefined) of
        undefined ->
            undefined;
        #xmlel{children = Children} = ErrorEl ->
            Code = exml_query:attr(ErrorEl, <<"code">>, undefined),
            Type = exml_query:attr(ErrorEl, <<"type">>, undefined),
            {Code, Type, Children}
    end.


%%% IQ stanzas
%%%-------------------------------------------------------------------

-spec iq(binary(), binary(), [#xmlel{} | #xmlcdata{}]) -> #xmlel{}.
iq(Id, Type, Children) ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"id">>, to_binary(Id)},
                    {<<"type">>, to_binary(Type)}],
           children = Children}.

-spec iq_get(binary(), binary(), [#xmlel{}]) -> #xmlel{}.
iq_get(Id, Xmlns, Children) ->
    iq(Id, <<"get">>, [#xmlel{name = <<"query">>,
                              attrs = [{<<"xmlns">>, to_binary(Xmlns)}],
                              children = Children}]).

-spec iq_set(binary(), binary(), [#xmlel{}]) -> #xmlel{}.
iq_set(Id, Xmlns, Children) ->
    iq(Id, <<"set">>, [#xmlel{name = <<"query">>,
                              attrs = [{<<"xmlns">>, to_binary(Xmlns)}],
                              children = Children}]).


%%% Presence stanzas
%%%-------------------------------------------------------------------

-spec presence(list()) -> #xmlel{}.
presence(PropList) ->
    Children = add_el_simple([<<"status">>,
                              <<"priority">>,
                              <<"show">>], PropList),
    el(<<"presence">>, [], Children).

-spec presence_roster(xcl:jid(), binary(), binary()) -> #xmlel{}.
presence_roster(To, Type, Priority) ->
    Attrs = [{<<"to">>, xcl_jid:bare_to_binary(To)},
             {<<"type">>, to_binary(Type)}],
    el(<<"presence">>, Attrs, [el_simple(<<"priority">>, Priority)]).


%%% Message stanzas
%%%-------------------------------------------------------------------

-spec message(binary(), binary(), xcl:jid(), list()) -> #xmlel{}.
message(Id, Type, To, PropList) ->
    Children = add_el_simple([<<"body">>,
                              <<"subject">>], PropList),
    Attrs = [{<<"id">>, to_binary(Id)},
             {<<"type">>, to_binary(Type)},
             {<<"to">>, xcl_jid:to_binary(To)}],
    el(<<"message">>, Attrs, Children).

-spec get_message_body(#xmlel{}) -> binary() | undefined.
get_message_body(El) ->
    case exml_query:subelement(El, <<"body">>, undefined) of
        undefined ->
            undefined;
        SubEl ->
            exml_query:cdata(SubEl)
    end.

-spec get_message_subject(#xmlel{}) -> binary() | undefined.
get_message_subject(El) ->
    case exml_query:subelement(El, <<"subject">>, undefined) of
        undefined ->
            undefined;
        SubEl ->
            exml_query:cdata(SubEl)
    end.


%%% Roster stanzas
%%%-------------------------------------------------------------------
%%%
%%% Set item tuple:  {xcl:jid(), Nick, [Group]}
%%% Remove item:     xcl:jid()
%%% Get item tuple:  {xcl:jid(), Nick, [Group], Subscription, Ask}

-spec roster_req_list(binary()) -> #xmlel{}.
roster_req_list(Id) ->
    iq_get(Id, ?NS_ROSTER, []).

-spec roster_set_item(binary(), {xcl:jid(), binary(), [binary()]}) -> #xmlel{}.
roster_set_item(Id, Item) ->
    roster_set_items(Id, [Item]).

-spec roster_set_items(binary(), [{xcl:jid(), binary(), [binary()]}]) -> #xmlel{}.
roster_set_items(Id, Items) ->
    iq_set(Id, ?NS_ROSTER, lists:map(fun roster_set_item/1, Items)).

-spec roster_set_item({xcl:jid(), binary(), [binary()]}) -> #xmlel{}.
roster_set_item({Jid, Name, Groups}) ->
    Attrs = [{<<"jid">>, xcl_jid:bare_to_binary(Jid)},
             {<<"name">>, Name},
             {<<"subscription">>, <<"add">>}],
    GroupEls = [el_simple(<<"group">>, G) || G <- Groups],
    el(<<"item">>, Attrs, GroupEls).

-spec roster_remove_item(binary(), xcl:jid()) -> #xmlel{}.
roster_remove_item(Id, Jid) ->
    roster_remove_items(Id, [Jid]).

-spec roster_remove_items(binary(), [xcl:jid()]) -> #xmlel{}.
roster_remove_items(Id, Jids) ->
    iq_set(Id, ?NS_ROSTER, lists:map(fun roster_remove_item/1, Jids)).

-spec roster_remove_item(xcl:jid()) -> #xmlel{}.
roster_remove_item(Jid) ->
    Attrs = [{<<"jid">>, xcl_jid:bare_to_binary(Jid)},
             {<<"subscription">>, <<"remove">>}],
    el(<<"item">>, Attrs, []).

-spec get_roster_list(#xmlel{}) -> [{binary(), xcl:jid(), binary(), list()}].
get_roster_list(El) ->
    Items = exml_query:paths(El, [{element, <<"query">>},
                                  {element, <<"item">>}]),
    lists:foldl(fun get_roster_item/2, [], Items).

-spec get_roster_item(#xmlel{}, [{xcl:jid(), binary(), binary(), [binary()]}]) ->
    [{xcl:jid(), binary(), binary(), [binary()]}].
get_roster_item(#xmlel{name = <<"item">>} = El, Acc) ->
    Jid = exml_query:attr(El, <<"jid">>, undefined),
    Name = exml_query:attr(El, <<"name">>, undefined),
    Sub = exml_query:attr(El, <<"subscription">>, <<"none">>),
    Ask = exml_query:attr(El, <<"ask">>, <<"none">>),
    Groups = [exml_query:cdata(GroupEl)
              || GroupEl <- exml_query:subelements(El, <<"group">>)],
    [{xcl_jid:from_binary(Jid), Name, Groups, Sub, Ask} | Acc].


%%% Privacy stanzas
%%%-------------------------------------------------------------------
%%%
%%% Set item tuple:  {Action, Type, Value, Order}
%%% Get item tuple:  {Action, Type, Value, Order}

-spec privacy_req_list_names(binary()) -> #xmlel{}.
privacy_req_list_names(Id) ->
    iq_get(Id, ?NS_PRIVACY, []).

-spec privacy_req_lists(binary(), [binary()]) -> #xmlel{}.
privacy_req_lists(Id, Lists) ->
    ListEls = [el(<<"list">>, [{<<"name">>, List}], []) || List <- Lists],
    iq_get(Id, ?NS_PRIVACY, ListEls).

-spec privacy_set_list(binary(), binary(), list()) -> #xmlel{}.
privacy_set_list(Id, Name, Items) ->
    ItemEls = lists:map(fun privacy_tuple_to_el/1, Items),
    AddEl = el(<<"list">>, [{<<"name">>, Name}], ItemEls),
    iq_set(Id, ?NS_PRIVACY, [AddEl]).

-spec privacy_tuple_to_el({binary(), binary(), binary(), binary()}) -> #xmlel{}.
privacy_tuple_to_el({Action, Type, Value, Order}) ->
    el(<<"item">>, [{<<"action">>, Action},
                    {<<"order">>, Order},
                    {<<"type">>, Type},
                    {<<"value">>, Value}], []).

-spec privacy_set_default(binary(), binary()) -> #xmlel{}.
privacy_set_default(Id, Name) ->
    privacy_set_list_type(Id, <<"default">>, Name).

-spec privacy_unset_default(binary()) -> #xmlel{}.
privacy_unset_default(Id) ->
    privacy_unset_list_type(Id, <<"default">>).

-spec privacy_set_active(binary(), binary()) -> #xmlel{}.
privacy_set_active(Id, Name) ->
    privacy_set_list_type(Id, <<"active">>, Name).

-spec privacy_unset_active(binary()) -> #xmlel{}.
privacy_unset_active(Id) ->
    privacy_unset_list_type(Id, <<"active">>).

-spec privacy_set_list_type(binary(), binary(), binary()) -> #xmlel{}.
privacy_set_list_type(Id, Type, Name) ->
    El = el(Type, [{<<"name">>, Name}], []),
    iq_set(Id, ?NS_PRIVACY, [El]).

-spec privacy_unset_list_type(binary(), binary()) -> #xmlel{}.
privacy_unset_list_type(Id, Type) ->
    El = el(Type, [], []),
    iq_set(Id, ?NS_PRIVACY, [El]).

-spec get_privacy_list_names(#xmlel{}) -> [{binary(), binary()}].
get_privacy_list_names(El) ->
    case exml_query:subelement(El, <<"query">>, undefined) of
        undefined ->
            [];
        #xmlel{children = Lists} ->
            lists:foldl(fun get_privacy_list_name/2, [], Lists)
    end.

-spec get_privacy_list_name(#xmlel{}, [{binary(), binary()}]) -> {binary(), binary()}.
get_privacy_list_name(#xmlel{name = Type} = El, Acc) ->
    case exml_query:attr(El, <<"name">>, undefined) of
        undefined ->
            Acc;
        Name ->
            [{Type, Name} | Acc]
    end;
get_privacy_list_name(_, Acc) ->
    Acc.

-spec get_privacy_list(#xmlel{}) -> undefined | {binary(), list()}.
get_privacy_list(El) ->
    case exml_query:path(El, [{element, <<"query">>},
                              {element, <<"list">>}], undefined) of
        undefined ->
            undefined;
        #xmlel{children = Items} = ListEl ->
            Name = exml_query:attr(ListEl, <<"name">>, undefined),
            {Name, lists:map(fun privacy_el_to_tuple/1, Items)}
    end.

-spec privacy_el_to_tuple(#xmlel{}) -> {binary(), binary(), binary(), binary()}.
privacy_el_to_tuple(#xmlel{name = <<"item">>} = El) ->
    Action = exml_query:attr(El, <<"action">>, undefined),
    Type = exml_query:attr(El, <<"type">>, undefined),
    Value = exml_query:attr(El, <<"value">>, undefined),
    Order = exml_query:attr(El, <<"order">>, undefined),
    {Action, Type, Value, Order}.


%%% MUC stanzas
%%%-------------------------------------------------------------------

-spec muc_join(xcl:jid(), list()) -> #xmlel{}.
muc_join(Room, PropList) ->
    Attrs = [{<<"to">>, xcl_jid:to_binary(Room)}],
    Children = add_el_simple([<<"status">>,
                              <<"priority">>], PropList),
    XmlnsEl = case proplists:get_value(<<"password">>, PropList) of
        undefined ->
            el_xmlns(?NS_MUC);
        Password ->
            el_xmlns(?NS_MUC, [el_simple(<<"password">>, Password)])
    end,
    el(<<"presence">>, Attrs, [XmlnsEl | Children]).

-spec muc_leave(xcl:jid(), binary()) -> #xmlel{}.
muc_leave(Room, Priority) ->
    Attrs = [{<<"to">>, xcl_jid:to_binary(Room)},
             {<<"type">>, <<"unavailable">>}],
    el(<<"presence">>, Attrs, [el_simple(<<"priority">>, Priority)]).

-spec muc_disco_info(binary(), xcl:jid()) -> #xmlel{}.
muc_disco_info(Id, Room) ->
    El = iq_get(Id, ?NS_DISCO_INFO, []),
    set_attr(El, attr(<<"to">>, xcl_jid:bare_to_binary(Room))).

-spec muc_disco_items(binary(), xcl:jid()) -> #xmlel{}.
muc_disco_items(Id, Room) ->
    El = iq_get(Id, ?NS_DISCO_ITEMS, []),
    set_attr(El, attr(<<"to">>, xcl_jid:to_binary(Room))).

-spec get_muc_count(#xmlel{}) -> binary() | undefined.
get_muc_count(El) ->
    FieldEls = exml_query:paths(El, [{element, <<"query">>},
                                     {element, <<"x">>},
                                     {element, <<"field">>}]),
    get_field(FieldEls, ?VAR_MUC_OCCUPANTS).

-spec get_field([#xmlel{}], binary()) -> binary() | undefined.
get_field([], _Var) ->
    undefined;
get_field([Field | Tail], Var) ->
    case get_value(Field, Var) of
        undefined ->
            get_field(Tail, Var);
        Value ->
            Value
    end.

-spec get_value(#xmlel{}, binary()) -> binary() | undefined.
get_value(#xmlel{children = [ValueEl]} = El, Var) ->
    case exml_query:attr(El, <<"var">>, undefined) of
        Var ->
            exml_query:cdata(ValueEl);
        _ ->
            undefined
    end;
get_value(_, _) ->
    undefined.

-spec get_muc_status_codes(#xmlel{}) -> [binary()].
get_muc_status_codes(El) ->
    case exml_query:subelement(El, <<"x">>, undefined) of
        undefined ->
            [];
        #xmlel{children = Children} = XEl ->
            case exml_query:attr(XEl, <<"xmlns">>, undefined) of
                ?NS_MUC_USER ->
                    lists:foldl(fun get_status/2, [], Children);
                undefined ->
                    undefined
            end
    end.

get_status(#xmlel{name = <<"status">>} = El, Acc) ->
    case exml_query:attr(El, <<"code">>, undefined) of
        undefined ->
            Acc;
        Code ->
            [Code | Acc]
    end;
get_status(_, Acc) ->
    Acc.


%%% Stream stanzas
%%%-------------------------------------------------------------------

-spec stream_start(binary(), client | server) -> xmlstreamelement().
stream_start(Server, client) ->
    stream_start2(Server, ?NS_JABBER_CLIENT);
stream_start(Server, server) ->
    stream_start2(Server, ?NS_JABBER_SERVER).

stream_start2(Server, Xmlns) ->
    #xmlstreamstart{name = <<"stream:stream">>,
                    attrs = [{<<"to">>, Server},
                             {<<"version">>, <<"1.0">>},
                             {<<"xml:lang">>, <<"en">>},
                             {<<"xmlns">>, Xmlns},
                             {<<"xmlns:stream">>, ?NS_XMPP}]}.

-spec stream_end() -> xmlstreamelement().
stream_end() ->
    #xmlstreamend{name = <<"stream:stream">>}.

-spec ws_open(binary()) -> #xmlel{}.
ws_open(Server) ->
    el(<<"open">>, [{<<"xmlns">>, ?NS_FRAMING},
                    {<<"to">>, Server},
                    {<<"version">>,<<"1.0">>}], []).

-spec ws_close() -> #xmlel{}. 
ws_close() ->
    el(<<"close">>, [{<<"xmlns">>, ?NS_FRAMING}], []).

-spec starttls() -> #xmlel{}.
starttls() ->
    el(<<"starttls">>, [{<<"xmlns">>, ?NS_TLS}], []).

-spec compress(binary() | atom()) -> #xmlel{}.
compress(Method) ->
    Method1 = to_binary(Method),
    el(<<"compress">>,
       [{<<"xmlns">>, ?NS_COMPRESS}],
       [el_simple(<<"method">>, Method1)]).

-spec auth(binary(), [#xmlel{} | #xmlcdata{}]) -> #xmlel{}.
auth(Mechanism, Body) ->
    el(<<"auth">>,
       [{<<"xmlns">>, ?NS_SASL}, {<<"mechanism">>, Mechanism}],
       Body).

-spec auth_plain(binary(), binary()) -> #xmlel{}.
auth_plain(Username, Password) ->
    Credentials = <<0:8,Username/binary, 0:8,Password/binary>>,
    auth(<<"PLAIN">>,
         [exml:escape_cdata(base64:encode(Credentials))]).

-spec bind(binary()) -> #xmlel{}.
bind(Resource) ->
    Bind = el(<<"bind">>,
              [{<<"xmlns">>, ?NS_BIND}],
              [el_simple(<<"resource">>, Resource)]),
    iq(id(), <<"set">>, [Bind]).

-spec session() -> #xmlel{}.
session() ->
    Session = el(<<"session">>, [{<<"xmlns">>, ?NS_SESSION}], []),
    iq(id(), <<"set">>, [Session]).

-spec get_stream_features(#xmlel{}) -> list().
get_stream_features(#xmlel{name = <<"stream:features">>, children = SubEls}) ->
    lists:foldl(fun get_stream_feature/2, [], SubEls);
get_stream_features(_) ->
    [].

-spec get_stream_feature(#xmlel{}, list()) -> list().
get_stream_feature(#xmlel{children = SubEls} = El, Features) ->
    case exml_query:attr(El, <<"xmlns">>) of
        ?NS_FEATURE_COMPRESS ->
            Compress = [exml_query:cdata(MethodEl) || MethodEl <- SubEls],
            [{server_compress, Compress} | Features];
        ?NS_TLS ->
            case exml_query:subelement(El, <<"required">>, undefined) of
                undefined -> [{server_starttls, offered} | Features];
                _         -> [{server_starttls, required} | Features]
            end;
        _ ->
            Features
    end.

-spec verify_starttls(#xmlel{}) -> boolean().
verify_starttls(#xmlel{name = <<"proceed">>,
                       attrs = [{<<"xmlns">>, ?NS_TLS}]}) ->
    true;
verify_starttls(_) ->
    false.

-spec verify_compress(#xmlel{}) -> boolean().
verify_compress(#xmlel{name = <<"compressed">>,
                       attrs = [{<<"xmlns">>, ?NS_COMPRESS}]}) ->
    true;
verify_compress(_) ->
    false.

-spec verify_auth(#xmlel{}) -> boolean().
verify_auth(#xmlel{name = <<"success">>}) ->
    true;
verify_auth(#xmlel{name = <<"failure">>}) ->
    false;
verify_auth(_) ->
    false.

-spec verify_bind(#xmlel{}) -> {true, xcl:jid()} | false.
verify_bind(#xmlel{name = <<"iq">>} = El) ->
    <<"result">> = exml_query:attr(El, <<"type">>),
    BindEl = exml_query:subelement(El, <<"bind">>),
    ?NS_BIND = exml_query:attr(BindEl, <<"xmlns">>),
    JidEl = exml_query:subelement(BindEl, <<"jid">>),
    BinJid = exml_query:cdata(JidEl),
    Jid = xcl_jid:from_binary(BinJid),
    {true, Jid};
verify_bind(_) ->
    false.


%%%===================================================================
%%% Private functions
%%%===================================================================

-spec add_el_simple([binary()], list()) -> [#xmlel{}].
add_el_simple(Keys, PropList) ->
    add_el_simple(Keys, PropList, []).

-spec add_el_simple([binary()], list(), [#xmlel{}]) -> [#xmlel{}].
add_el_simple([], _PropList, Acc) ->
    Acc;
add_el_simple([Key | Tail], PropList, Acc) ->
    case proplists:get_value(Key, PropList) of
        undefined ->
            add_el_simple(Tail, PropList, Acc);
        Value ->
            add_el_simple(Tail, PropList, [el_simple(Key, Value) | Acc])
    end.

