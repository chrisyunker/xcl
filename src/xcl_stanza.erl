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

-export([el_xmlns/1, el_xmlns/2,
         get_sender/1,
         set_sender/2,
         get_recipient/1,
         set_recipient/2,
         get_id/1,
         get_type/1,
         get_xmlns/1,
         get_error/1]).

-export([iq/3,
         iq_get/3,
         iq_set/3,
         get_iq_result/1,
         presence/1,
         presence_roster/3,
         get_presence_type/1,
         get_presence_status/1,
         get_presence_show/1,
         message/4,
         get_message_type/1,
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
         session/1,
         get_stream_features/1,
         verify_starttls/1,
         verify_compress/1,
         verify_auth/1,
         verify_bind/1,
         verify_session/1]).

-import(xcl_util, [to_binary/1,
                   id/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%% Core functions
%%%-------------------------------------------------------------------

-spec el_xmlns(binary() | string()) -> exml:element().
el_xmlns(Xmlns) ->
    xcl_xml:el(<<"x">>, [xcl_xml:attr(<<"xmlns">>, Xmlns)], []).

-spec el_xmlns(binary() | string(), [exml:element() | exml:cdata()]) ->
    exml:element().
el_xmlns(Xmlns, Children) ->
    xcl_xml:el(<<"x">>, [xcl_xml:attr(<<"xmlns">>, Xmlns)], Children).

-spec get_sender(exml:element()) -> binary() | undefined.
get_sender(El) ->
    exml_query:attr(El, <<"from">>, undefined).

-spec set_sender(exml:element(), binary()) -> exml:element().
set_sender(El, Sender) ->
    xcl_xml:set_attr(El, {<<"from">>, Sender}).

-spec get_recipient(exml:element()) -> binary() | undefined.
get_recipient(El) ->
    exml_query:attr(El, <<"to">>, undefined).

-spec set_recipient(exml:element(), binary()) -> exml:element().
set_recipient(El, Recipient) ->
    xcl_xml:set_attr(El, {<<"to">>, Recipient}).

-spec get_id(exml:element()) -> binary() | undefined.
get_id(El) ->
    exml_query:attr(El, <<"id">>, undefined).

-spec get_type(exml:element()) -> binary() | undefined.
get_type(El) ->
    exml_query:attr(El, <<"type">>, undefined).

-spec get_xmlns(exml:element()) -> binary() | undefined.
get_xmlns(El) ->
    case exml_query:subelement(El, <<"x">>, undefined) of
        undefined ->
            undefined;
        XEl ->
            exml_query:attr(XEl, <<"xmlns">>, undefined)
    end.

-spec get_error(exml:element()) ->
    {binary(), binary(), [exml:element()]} | undefined.
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

-spec iq(binary(), binary(), [exml:element() | exml:cdata()]) -> exml:element().
iq(Id, Type, Children) ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"id">>, to_binary(Id)},
                    {<<"type">>, to_binary(Type)}],
           children = Children}.

-spec iq_get(binary(), binary(), [exml:element()]) -> exml:element().
iq_get(Id, Xmlns, Children) ->
    iq(Id, <<"get">>, [#xmlel{name = <<"query">>,
                              attrs = [{<<"xmlns">>, to_binary(Xmlns)}],
                              children = Children}]).

-spec iq_set(binary(), binary(), [exml:element()]) -> exml:element().
iq_set(Id, Xmlns, Children) ->
    iq(Id, <<"set">>, [#xmlel{name = <<"query">>,
                              attrs = [{<<"xmlns">>, to_binary(Xmlns)}],
                              children = Children}]).

-spec get_iq_result(exml:element()) -> exml:element() | undefined.
get_iq_result(#xmlel{name = <<"iq">>, children = Result} = El) ->
    case get_type(El) of
        <<"result">> -> Result;
        _            -> undefined
    end.

%%% Presence stanzas
%%%-------------------------------------------------------------------

-spec presence(list()) -> exml:element().
presence(PropList) ->
    Children = add_el_simple([<<"status">>,
                              <<"priority">>,
                              <<"show">>], PropList),
    xcl_xml:el(<<"presence">>, [], Children).

-spec presence_roster(xcl:jid(), binary(), binary()) -> exml:element().
presence_roster(To, Type, Priority) ->
    Attrs = [{<<"to">>, xcl_jid:bare_to_binary(To)},
             {<<"type">>, to_binary(Type)}],
    xcl_xml:el(<<"presence">>, Attrs,
               [xcl_xml:el_simple(<<"priority">>, Priority)]).

-spec get_presence_type(exml:element()) -> binary() | undefined.
get_presence_type(#xmlel{name = <<"presence">>} = El) ->
    exml_query:attr(El, <<"type">>, undefined).

-spec get_presence_status(exml:element()) -> binary() | undefined.
get_presence_status(#xmlel{name = <<"presence">>} = El) ->
    case xcl_xml:get_subelement(El, <<"status">>) of
        undefined -> undefined;
        StatusEl -> exml_query:cdata(StatusEl)
    end.

-spec get_presence_show(exml:element()) -> binary() | undefined.
get_presence_show(#xmlel{name = <<"presence">>} = El) ->
    case xcl_xml:get_subelement(El, <<"show">>) of
        undefined -> undefined;
        ShowEl -> exml_query:cdata(ShowEl)
    end.

%%% Message stanzas
%%%-------------------------------------------------------------------

-spec message(binary(), binary(), xcl:jid(), list()) -> exml:element().
message(Id, Type, To, PropList) ->
    Children = add_el_simple([<<"body">>,
                              <<"subject">>], PropList),
    Attrs = [{<<"id">>, to_binary(Id)},
             {<<"type">>, to_binary(Type)},
             {<<"to">>, xcl_jid:to_binary(To)}],
    xcl_xml:el(<<"message">>, Attrs, Children).

-spec get_message_type(exml:element()) -> binary() | undefined.
get_message_type(#xmlel{name = <<"message">>} = El) ->
    exml_query:attr(El, <<"type">>, undefined).

-spec get_message_body(exml:element()) -> binary() | undefined.
get_message_body(El) ->
    case exml_query:subelement(El, <<"body">>, undefined) of
        undefined ->
            undefined;
        SubEl ->
            exml_query:cdata(SubEl)
    end.

-spec get_message_subject(exml:element()) -> binary() | undefined.
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

-spec roster_req_list(binary()) -> exml:element().
roster_req_list(Id) ->
    iq_get(Id, ?NS_ROSTER, []).

-spec roster_set_item(binary(), {xcl:jid(), binary(), [binary()]}) ->
    exml:element().
roster_set_item(Id, Item) ->
    roster_set_items(Id, [Item]).

-spec roster_set_items(binary(), [{xcl:jid(), binary(), [binary()]}]) ->
    exml:element().
roster_set_items(Id, Items) ->
    iq_set(Id, ?NS_ROSTER, lists:map(fun roster_set_item/1, Items)).

-spec roster_set_item({xcl:jid(), binary(), [binary()]}) -> exml:element().
roster_set_item({Jid, Name, Groups}) ->
    Attrs = [{<<"jid">>, xcl_jid:bare_to_binary(Jid)},
             {<<"name">>, Name},
             {<<"subscription">>, <<"add">>}],
    GroupEls = [xcl_xml:el_simple(<<"group">>, G) || G <- Groups],
    xcl_xml:el(<<"item">>, Attrs, GroupEls).

-spec roster_remove_item(binary(), xcl:jid()) -> exml:element().
roster_remove_item(Id, Jid) ->
    roster_remove_items(Id, [Jid]).

-spec roster_remove_items(binary(), [xcl:jid()]) -> exml:element().
roster_remove_items(Id, Jids) ->
    iq_set(Id, ?NS_ROSTER, lists:map(fun roster_remove_item/1, Jids)).

-spec roster_remove_item(xcl:jid()) -> exml:element().
roster_remove_item(Jid) ->
    Attrs = [{<<"jid">>, xcl_jid:bare_to_binary(Jid)},
             {<<"subscription">>, <<"remove">>}],
    xcl_xml:el(<<"item">>, Attrs, []).

-spec get_roster_list(exml:element()) ->
    [{binary(), xcl:jid(), binary(), list()}].
get_roster_list(El) ->
    Items = exml_query:paths(El, [{element, <<"query">>},
                                  {element, <<"item">>}]),
    lists:foldl(fun get_roster_item/2, [], Items).

-spec get_roster_item(exml:element(),
                      [{xcl:jid(), binary(), binary(), [binary()]}]) ->
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

-spec privacy_req_list_names(binary()) -> exml:element().
privacy_req_list_names(Id) ->
    iq_get(Id, ?NS_PRIVACY, []).

-spec privacy_req_lists(binary(), [binary()]) -> exml:element().
privacy_req_lists(Id, Lists) ->
    ListEls = [xcl_xml:el(<<"list">>,
                          [{<<"name">>, List}], []) || List <- Lists],
    iq_get(Id, ?NS_PRIVACY, ListEls).

-spec privacy_set_list(binary(), binary(), list()) -> exml:element().
privacy_set_list(Id, Name, Items) ->
    ItemEls = lists:map(fun privacy_tuple_to_el/1, Items),
    AddEl = xcl_xml:el(<<"list">>, [{<<"name">>, Name}], ItemEls),
    iq_set(Id, ?NS_PRIVACY, [AddEl]).

-spec privacy_tuple_to_el({binary(), binary(), binary(), binary()}) ->
    exml:element().
privacy_tuple_to_el({Action, Type, Value, Order}) ->
    xcl_xml:el(<<"item">>, [{<<"action">>, Action},
                            {<<"order">>, Order},
                            {<<"type">>, Type},
                            {<<"value">>, Value}], []).

-spec privacy_set_default(binary(), binary()) -> exml:element().
privacy_set_default(Id, Name) ->
    privacy_set_list_type(Id, <<"default">>, Name).

-spec privacy_unset_default(binary()) -> exml:element().
privacy_unset_default(Id) ->
    privacy_unset_list_type(Id, <<"default">>).

-spec privacy_set_active(binary(), binary()) -> exml:element().
privacy_set_active(Id, Name) ->
    privacy_set_list_type(Id, <<"active">>, Name).

-spec privacy_unset_active(binary()) -> exml:element().
privacy_unset_active(Id) ->
    privacy_unset_list_type(Id, <<"active">>).

-spec privacy_set_list_type(binary(), binary(), binary()) -> exml:element().
privacy_set_list_type(Id, Type, Name) ->
    El = xcl_xml:el(Type, [{<<"name">>, Name}], []),
    iq_set(Id, ?NS_PRIVACY, [El]).

-spec privacy_unset_list_type(binary(), binary()) -> exml:element().
privacy_unset_list_type(Id, Type) ->
    El = xcl_xml:el(Type, [], []),
    iq_set(Id, ?NS_PRIVACY, [El]).

-spec get_privacy_list_names(exml:element()) -> [{binary(), binary()}].
get_privacy_list_names(El) ->
    case exml_query:subelement(El, <<"query">>, undefined) of
        undefined ->
            [];
        #xmlel{children = Lists} ->
            lists:foldl(fun get_privacy_list_name/2, [], Lists)
    end.

-spec get_privacy_list_name(exml:element(), [{binary(), binary()}]) ->
    {binary(), binary()}.
get_privacy_list_name(#xmlel{name = Type} = El, Acc) ->
    case exml_query:attr(El, <<"name">>, undefined) of
        undefined ->
            Acc;
        Name ->
            [{Type, Name} | Acc]
    end;
get_privacy_list_name(_, Acc) ->
    Acc.

-spec get_privacy_list(exml:element()) -> undefined | {binary(), list()}.
get_privacy_list(El) ->
    case exml_query:path(El, [{element, <<"query">>},
                              {element, <<"list">>}], undefined) of
        undefined ->
            undefined;
        #xmlel{children = Items} = ListEl ->
            Name = exml_query:attr(ListEl, <<"name">>, undefined),
            {Name, lists:map(fun privacy_el_to_tuple/1, Items)}
    end.

-spec privacy_el_to_tuple(exml:element()) ->
    {binary(), binary(), binary(), binary()}.
privacy_el_to_tuple(#xmlel{name = <<"item">>} = El) ->
    Action = exml_query:attr(El, <<"action">>, undefined),
    Type = exml_query:attr(El, <<"type">>, undefined),
    Value = exml_query:attr(El, <<"value">>, undefined),
    Order = exml_query:attr(El, <<"order">>, undefined),
    {Action, Type, Value, Order}.


%%% MUC stanzas
%%%-------------------------------------------------------------------

-spec muc_join(xcl:jid(), list()) -> exml:element().
muc_join(Room, PropList) ->
    Attrs = [{<<"to">>, xcl_jid:to_binary(Room)}],
    Children = add_el_simple([<<"status">>,
                              <<"priority">>], PropList),
    XmlnsEl = case proplists:get_value(<<"password">>, PropList) of
        undefined ->
            el_xmlns(?NS_MUC);
        Password ->
            el_xmlns(?NS_MUC, [xcl_xml:el_simple(<<"password">>, Password)])
    end,
    xcl_xml:el(<<"presence">>, Attrs, [XmlnsEl | Children]).

-spec muc_leave(xcl:jid(), binary()) -> exml:element().
muc_leave(Room, Priority) ->
    Attrs = [{<<"to">>, xcl_jid:to_binary(Room)},
             {<<"type">>, <<"unavailable">>}],
    xcl_xml:el(<<"presence">>, Attrs,
               [xcl_xml:el_simple(<<"priority">>, Priority)]).

-spec muc_disco_info(binary(), xcl:jid()) -> exml:element().
muc_disco_info(Id, Room) ->
    El = iq_get(Id, ?NS_DISCO_INFO, []),
    xcl_xml:set_attr(El, xcl_xml:attr(<<"to">>, xcl_jid:bare_to_binary(Room))).

-spec muc_disco_items(binary(), xcl:jid()) -> exml:element().
muc_disco_items(Id, Room) ->
    El = iq_get(Id, ?NS_DISCO_ITEMS, []),
    xcl_xml:set_attr(El, xcl_xml:attr(<<"to">>, xcl_jid:to_binary(Room))).

-spec get_muc_count(exml:element()) -> binary() | undefined.
get_muc_count(El) ->
    FieldEls = exml_query:paths(El, [{element, <<"query">>},
                                     {element, <<"x">>},
                                     {element, <<"field">>}]),
    get_field(FieldEls, ?VAR_MUC_OCCUPANTS).

-spec get_field([exml:element()], binary()) -> binary() | undefined.
get_field([], _Var) ->
    undefined;
get_field([Field | Tail], Var) ->
    case get_value(Field, Var) of
        undefined ->
            get_field(Tail, Var);
        Value ->
            Value
    end.

-spec get_value(exml:element(), binary()) -> binary() | undefined.
get_value(#xmlel{children = [ValueEl]} = El, Var) ->
    case exml_query:attr(El, <<"var">>, undefined) of
        Var ->
            exml_query:cdata(ValueEl);
        _ ->
            undefined
    end;
get_value(_, _) ->
    undefined.

-spec get_muc_status_codes(exml:element()) -> [binary()].
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

-spec stream_start(binary(), client | server) -> exml_stream:element().
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

-spec stream_end() -> exml_stream:element().
stream_end() ->
    #xmlstreamend{name = <<"stream:stream">>}.

-spec ws_open(binary()) -> exml:element().
ws_open(Server) ->
    xcl_xml:el(<<"open">>, [{<<"xmlns">>, ?NS_FRAMING},
                            {<<"to">>, Server},
                            {<<"version">>, <<"1.0">>}], []).

-spec ws_close() -> exml:element().
ws_close() ->
    xcl_xml:el(<<"close">>, [{<<"xmlns">>, ?NS_FRAMING}], []).

-spec starttls() -> exml:element().
starttls() ->
    xcl_xml:el(<<"starttls">>, [{<<"xmlns">>, ?NS_TLS}], []).

-spec compress(binary() | atom()) -> exml:element().
compress(Method) ->
    Method1 = to_binary(Method),
    xcl_xml:el(<<"compress">>,
               [{<<"xmlns">>, ?NS_COMPRESS}],
               [xcl_xml:el_simple(<<"method">>, Method1)]).

-spec auth(binary(), [exml:element() | exml:cdata()]) -> exml:element().
auth(Mechanism, Body) ->
    xcl_xml:el(<<"auth">>,
               [{<<"xmlns">>, ?NS_SASL}, {<<"mechanism">>, Mechanism}],
               Body).

-spec auth_plain(binary(), binary()) -> exml:element().
auth_plain(Username, Password) ->
    Credentials = <<0:8, Username/binary, 0:8, Password/binary>>,
    auth(<<"PLAIN">>,
         [exml:escape_cdata(base64:encode(Credentials))]).

-spec bind(binary()) -> exml:element().
bind(Resource) ->
    Bind = xcl_xml:el(<<"bind">>,
                      [{<<"xmlns">>, ?NS_BIND}],
                      [xcl_xml:el_simple(<<"resource">>, Resource)]),
    iq(id(), <<"set">>, [Bind]).

-spec session() -> exml:element().
session() ->
    Session = xcl_xml:el(<<"session">>, [{<<"xmlns">>, ?NS_SESSION}], []),
    iq(id(), <<"set">>, [Session]).

-spec session(list()) -> exml:element().
session(Params) ->
    ParamEls = [xcl_xml:el_simple(K, to_binary(V)) || {K, V} <- Params],
    Session = xcl_xml:el(<<"session">>, [{<<"xmlns">>, ?NS_SESSION}], ParamEls),
    iq(id(), <<"set">>, [Session]).

-spec get_stream_features(exml:element()) -> list().
get_stream_features(#xmlel{name = <<"stream:features">>, children = SubEls}) ->
    lists:foldl(fun get_stream_feature/2, [], SubEls);
get_stream_features(_) ->
    [].

-spec get_stream_feature(exml:element(), list()) -> list().
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

-spec verify_starttls(exml:element()) -> boolean().
verify_starttls(#xmlel{name = <<"proceed">>,
                       attrs = [{<<"xmlns">>, ?NS_TLS}]}) ->
    true;
verify_starttls(_) ->
    false.

-spec verify_compress(exml:element()) -> boolean().
verify_compress(#xmlel{name = <<"compressed">>,
                       attrs = [{<<"xmlns">>, ?NS_COMPRESS}]}) ->
    true;
verify_compress(_) ->
    false.

-spec verify_auth(exml:element()) -> boolean().
verify_auth(#xmlel{name = <<"success">>}) ->
    true;
verify_auth(#xmlel{name = <<"failure">>}) ->
    false;
verify_auth(_) ->
    false.

-spec verify_bind(exml:element()) -> {true, xcl:jid()} | false.
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

-spec verify_session(exml:element()) -> {true, list()} | false.
verify_session(#xmlel{name = <<"iq">>} = El) ->
    <<"result">> = exml_query:attr(El, <<"type">>),
    SessionEl = exml_query:subelement(El, <<"session">>),
    ?NS_SESSION = exml_query:attr(SessionEl, <<"xmlns">>),
    Params = get_el_simple(SessionEl#xmlel.children),
    {true, Params};
verify_session(_) ->
    false.

%%%===================================================================
%%% Private functions
%%%===================================================================

-spec add_el_simple([binary()], list()) -> [exml:element()].
add_el_simple(Keys, PropList) ->
    add_el_simple(Keys, PropList, []).

-spec add_el_simple([binary()], list(), [exml:element()]) -> [exml:element()].
add_el_simple([], _PropList, Acc) ->
    Acc;
add_el_simple([Key | Tail], PropList, Acc) ->
    case proplists:get_value(Key, PropList) of
        undefined ->
            add_el_simple(Tail, PropList, Acc);
        Value ->
            add_el_simple(Tail, PropList, [xcl_xml:el_simple(Key, Value) | Acc])
    end.

-spec get_el_simple([exml:element()]) -> list().
get_el_simple(Els) ->
    get_el_simple(Els, []).

get_el_simple([], Acc) ->
    Acc;
get_el_simple([El | Tail], Acc) ->
    case exml_query:cdata(El) of
        <<>> ->
            get_el_simple(Tail, Acc);
        Value ->
            get_el_simple(Tail, [{El#xmlel.name, Value} | Acc])
    end.
