%%%-------------------------------------------------------------------
%%% @author Chris Yunker <chris@yunker.io>
%%% @copyright (C) 2014, Chris Yunker
%%% @doc XMPP client session
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xcl_session).

-include_lib("exml/include/exml_stream.hrl").
-include("xcl.hrl").
-include("xcl_config.hrl").

-export([start/1,
         stop/1,
         send_stanza/2,
         receive_stanza/1, receive_stanza/2,
         is_valid/1,
         get_jid/1]).

-import(xcl_util, [to_binary/1]).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec start(list()) -> {ok, xcl:session()} | {error, term()}.
start(Args) ->
    xcl_log:debug("[xcl_session] Starting session"),
    case connect(Args) of
        {ok, Session} ->
            try
                Features = start_stream(Session),
                Session1 = negotiate_tls(Session, Args, Features),
                Session2 = negotiate_compression(Session1, Args, Features),
                Session3 = authenticate(Session2, Args),
                Session4 = bind(Session3, Args),
                session(Session4),
                {ok, Session4}
            catch
                throw:{session_error, Error} ->
                    stop(Session),
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

-spec stop(xcl:session()) -> ok | invalid_session | {error, term()}.
stop(#session{transport = Trans} = Session) ->
    xcl_log:debug("[xcl_session] Stopping session"),
    case is_valid(Session) of
        true ->
            try
                Trans:send_stanza(Session, xcl_stanza:stream_end()),
                {ok, _StreamEl} = receive_stanza(Session, wait_for_stream_end),
                xcl_log:debug("[xcl_session] Server ended stream"),
                ok
            catch
                _:Reason ->
                    Trans:disconnect(Session),
                    {error, {disconnect_error, Reason}}
            end;
        false ->
            invalid_session 
    end.

-spec send_stanza(xcl:session(), xmlstreamelement()) -> ok | {error, term()}.
send_stanza(#session{transport = Trans} = Session, El) ->
    Trans:send_stanza(Session, El).

-spec receive_stanza(xcl:session()) -> {ok, xmlstreamelement()}.
receive_stanza(Session) ->
    receive_stanza(Session, undefined).

-spec receive_stanza(xcl:session(), atom()) -> {ok, xmlstreamelement()}.
receive_stanza(#session{pid = Pid}, Name) ->
    receive
        {stanza, Pid, Stanza} ->
            {ok, Stanza}
    after
        ?XCL_RECV_TIMEOUT ->
            case Name of
                undefined -> throw(receive_stanza_timeout);
                _ -> throw({receive_stanza_timeout, Name})
            end
    end.

-spec is_valid(xcl:session()) -> boolean().
is_valid(#session{transport = Trans} = Session) ->
    Trans:is_connected(Session).

-spec get_jid(xcl:session()) -> xcl:jid().
get_jid(#session{jid = Jid}) ->
    Jid.

%%%===================================================================
%%% Private functions
%%%===================================================================

-spec connect(list()) -> {ok, xcl:session()} | {error, term()}.
connect(Args) ->
    try
        check_args(Args),
        TransType = proplists:get_value(transport, Args, ?XCL_DEFAULT_TRANS),
        Trans = transport_module(TransType),
        case Trans:connect(Args) of
            {ok, Session} ->
                {ok, Session#session{jid = create_jid(Args)}};
            {error, Error} ->
                {error, {connect_error, Error}}
        end
    catch
        _:Reason ->
            {error, {connect_error, Reason}}
    end.

-spec start_stream(xcl:session()) -> list().
start_stream(#session{transport = Trans, jid = Jid} = Session) ->
    xcl_log:debug("[xcl_session] Starting stream"),
    try
        Trans:send_stanza(Session,
                          xcl_stanza:stream_start(Jid#jid.domain, client)),
        {ok, _StreamEl} = receive_stanza(Session, wait_for_stream_start),
        {ok, FeaturesEl} = receive_stanza(Session, wait_for_features),
        Features = xcl_stanza:get_stream_features(FeaturesEl),
        xcl_log:debug("[xcl_session] Stream features: ~p", [Features]),
        Features
    catch
        _:Reason ->
            throw({session_error, {start_stream_error, Reason}})
    end.

-spec authenticate(xcl:session(), list()) -> xcl:session().
authenticate(#session{transport = Trans} = Session, Args) ->
    try
        xcl_auth:authenticate(Session, Args),
        Trans:reset_parser(Session),
        start_stream(Session),
        xcl_log:debug("[xcl_session] Client authenticated"),
        Session
    catch
        _:Reason ->
            throw({session_error, {authenticate_error, Reason}})
    end.

-spec bind(xcl:session(), list()) -> xcl:session().
bind(#session{transport = Trans} = Session, Args) ->
    try
        Resource = to_binary(proplists:get_value(resource, Args)),
        Trans:send_stanza(Session, xcl_stanza:bind(Resource)),
        {ok, BindEl} = receive_stanza(Session, wait_for_bind),
        Jid = case xcl_stanza:verify_bind(BindEl) of
            {true, BindJid} -> BindJid;
            false -> throw({fail_bind, BindEl})
        end,
        xcl_log:debug("[xcl_session] Binded to jid: ~s", [xcl_jid:to_binary(Jid)]),
        Session#session{jid = Jid}
    catch
        _:Reason ->
            throw({session_error, {bind_error, Reason}})
    end.

-spec session(xcl:session()) -> ok.
session(#session{transport = Trans} = Session) ->
    try
        Trans:send_stanza(Session, xcl_stanza:session()),
        {ok, _SessionEl} = receive_stanza(Session, wait_for_session),
        xcl_log:debug("[xcl_session] Session established"),
        ok
    catch
        _:Reason ->
            throw({session_error, {session_error, Reason}})
    end.

-spec negotiate_tls(xcl:session(), list(), list()) -> xcl:session().
negotiate_tls(Session, Args, Features) ->
    xcl_log:debug("[xcl_session] Negotiate TLS"),
    %% client: none | starttls | tls
    %% server: none | offered | required
    ClientSsl = proplists:get_value(tls, Args),
    ServerSsl = proplists:get_value(server_starttls, Features, none),
    try
        case {ClientSsl, ServerSsl} of
            {tls, _} ->             Session;
            {starttls, none} ->     Session;
            {starttls, _} ->        enable_tls(Session);
            {none, required} ->     throw("Server requires TLS, "
                                          "client does not support it");
            {none, _} ->            Session
        end
    catch
        _:Reason ->
            throw({session_error, {tls_error, Reason}})
    end.

-spec enable_tls(xcl:session()) -> xcl:session().
enable_tls(#session{transport = Trans} = Session) ->
    xcl_log:debug("[xcl_session] Enabling TLS"),
    Trans:send_stanza(Session, xcl_stanza:starttls()),
    {ok, TlsEl} = receive_stanza(Session, enable_tls),
    case xcl_stanza:verify_starttls(TlsEl) of
        true -> ok;
        false -> throw({fail_enable_tls, TlsEl})
    end,
    Session1 = Trans:enable_tls(Session),
    start_stream(Session1),
    xcl_log:debug("[xcl_session] TLS enabled"),
    Session1.

-spec negotiate_compression(xcl:session(), list(), list()) -> xcl:session().
negotiate_compression(Session, _Args, _Features) ->
    %% Not supported yet
    Session.

-spec check_args(list()) -> ok.
check_args(Args) ->
    ReqArgs = [username,
               password,
               host,
               port,
               domain,
               resource],
    lists:foreach(fun(ReqArg) ->
                case lists:keyfind(ReqArg, 1, Args) of
                    false -> throw({missing_argument, ReqArg});
                    _ -> ok
                end
        end, ReqArgs).

-spec transport_module(atom()) -> atom().
transport_module(socket) -> xcl_socket;
transport_module(bosh) ->   xcl_bosh;
transport_module(ws) ->     xcl_ws.

-spec create_jid(list()) -> xcl:jid().
create_jid(Args) ->
    xcl_jid:make(proplists:get_value(username, Args),
                 proplists:get_value(domain, Args),
                 proplists:get_value(resource, Args)).

