-module(xcl_socket_client).

-include("xcl.hrl").
-include("xcl_config.hrl").

-export([test_roster/1,
         test_privacy/1,
         test_muc/1]).

-export([connect/3,
         get_roster_list/1,
         get_privacy_lists/1,
         get_privacy_list/2,
         disconnect/1]).

-define(EXL_USERNAME_1, "user1").
-define(EXL_PASSWORD_1, "password1").
-define(EXL_USERNAME_2, "user2").
-define(EXL_PASSWORD_2, "password1").
-define(EXL_DOMAIN, "yunker.io").
-define(EXL_MUC_DOMAIN, "conference.yunker.io").
-define(EXL_RESOURCE, "xcl").
-define(EXL_HOST, "localhost").
-define(EXL_LOG_LEVEL, debug).


%%%===================================================================
%%% API
%%%===================================================================

test_roster(Type) ->
    {ok, S1} = connect(Type, ?EXL_USERNAME_1, ?EXL_PASSWORD_1),
    get_roster_list(S1),
    disconnect(S1).

test_privacy(Type) ->
    {ok, S1} = connect(Type, ?EXL_USERNAME_1, ?EXL_PASSWORD_1),
    Lists = get_privacy_lists(S1),
    {_, DefaultList} = lists:keyfind(<<"default">>, 1, Lists),
    get_privacy_list(S1, DefaultList),
    disconnect(S1).

test_muc(Type) ->
    {ok, S1} = connect(Type, ?EXL_USERNAME_1, ?EXL_PASSWORD_1),
    {ok, S2} = connect(Type, ?EXL_USERNAME_2, ?EXL_PASSWORD_2),
    Room = xcl_jid:make(<<"room1">>, ?EXL_MUC_DOMAIN, <<"nick1">>),
    muc_join(S1, Room),
    muc_disco(S2, Room),
    disconnect(S1),
    disconnect(S2).

muc_join(Session, Room) ->
    Stanza = xcl_stanza:muc_join(Room, [{<<"status">>, <<"ready">>},
                                        {<<"priority">>, <<"0">>}]),
    xcl_session:send_stanza(Session, Stanza),
    {ok, Result} = xcl_session:receive_stanza(Session),
    Codes = xcl_stanza:get_muc_status_codes(Result),
    xcl_log:debug("Joined room [~s] with status codes ~p",
                  [xcl_jid:bare_to_binary(Room), Codes]),
    Codes.

muc_disco(Session, Room) ->
    Stanza = xcl_stanza:muc_disco_info(<<"disco_1">>, Room),
    xcl_session:send_stanza(Session, Stanza),
    {ok, Result} = xcl_session:receive_stanza(Session),
    Count = xcl_stanza:get_muc_count(Result),
    xcl_log:debug("Room [~s] has count [~p]",
                  [xcl_jid:bare_to_binary(Room), xcl_util:to_integer(Count)]),
    Count.

connect(Type, Username, Password) ->
    start_lager(),
    {Tls, Port} = case Type of
        plain ->    {none, 5222};
        starttls -> {starttls, 5222};
        tls ->      {tls, 5223}
    end,
    Args = [{username, Username},
            {domain, ?EXL_DOMAIN},
            {resource, ?EXL_RESOURCE},
            {password, Password},
            {host, ?EXL_HOST},
            {port, Port},
            {transport, socket},
            {auth, plain},
            {tls, Tls},
            {compress, none}],
    case xcl_session:start(Args) of
        {ok, Session} ->
            {ok, Session};
        {error, Error} ->
            xcl_log:error("Failed to start session: ~p", [Error]),
            {error, Error}
    end.

get_roster_list(Session) ->
    xcl_log:debug("Getting roster..."),
    xcl_session:send_stanza(Session, xcl_stanza:roster_req_list(<<"req_rost_1">>)),
    {ok, RosterEl} = xcl_session:receive_stanza(Session),
    Roster = xcl_stanza:get_roster_list(RosterEl),
    xcl_log:debug("Roster ~p", [Roster]),
    Roster.

get_privacy_lists(Session) ->
    xcl_log:debug("Getting privacy lists..."),
    xcl_session:send_stanza(Session, xcl_stanza:privacy_req_list_names(<<"req_priv_1">>)),
    {ok, El} = xcl_session:receive_stanza(Session),
    PrivacyLists = xcl_stanza:get_privacy_list_names(El),
    xcl_log:debug("Privacy lists ~p", [PrivacyLists]),
    PrivacyLists.

get_privacy_list(Session, Name) ->
    xcl_log:debug("Getting privacy list [~s]...", [Name]),
    xcl_session:send_stanza(Session,
                            xcl_stanza:privacy_req_lists(<<"req_priv_2">>,
                                                         [xcl_util:to_binary(Name)])),
    {ok, El} = xcl_session:receive_stanza(Session),
    PrivacyList = xcl_stanza:get_privacy_list(El),
    xcl_log:debug("Privacy list [~s] ~p", [Name, PrivacyList]),
    PrivacyList.

disconnect(Session) ->
    xcl_log:debug("Stopping session..."),
    xcl_session:stop(Session).

start_lager() ->
    case lists:keyfind(lager, 1, application:which_applications()) of
        {lager, _, _} ->
            ok;
        _ ->
            lager:start(),
            lager:set_loglevel(lager_console_backend, ?EXL_LOG_LEVEL),
            xcl_log:debug("Started lager")
    end.

