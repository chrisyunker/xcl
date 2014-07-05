%%%-------------------------------------------------------------------
%%% @author Chris Yunker <chris@yunker.io>
%%% @copyright (C) 2014, Chris Yunker
%%% @doc WebSocket transport implementation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xcl_ws).

-include_lib("exml/include/exml_stream.hrl").
-include("xcl.hrl").
-include("xcl_config.hrl").

-behaviour(gen_server).

%% transport implementation
-export([check_args/1,
         connect/1,
         disconnect/1,
         start_stream/1,
         end_stream/1,
         send_stanza/2,
         enable_tls/1,
         reset_parser/1,
         is_connected/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-import(xcl_util, [to_list/1, to_integer/1]).

-record(state, {client :: pid(),
                ws :: pid(),
                parser :: tuple(),
                compress = none :: atom(),
                legacy_ws = false :: boolean()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec check_args(list()) -> ok.
check_args(Args) ->
    ReqArgs = [username,
               password,
               host,
               port,
               domain,
               resource,
               path],
    lists:foreach(fun(ReqArg) ->
                case lists:keyfind(ReqArg, 1, Args) of
                    false -> throw({missing_argument, ReqArg});
                    _ -> ok
                end
        end, ReqArgs).

-spec connect(list()) -> {ok, xcl:session()} | {error, term()}.
connect(Args) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [self()], []),
    gen_server:call(Pid, {connect, Args}, ?XCL_TRANS_CONN_TIMEOUT).

-spec disconnect(xcl:session()) -> ok | not_connected.
disconnect(#session{pid = Pid}) ->
    try
        gen_server:call(Pid, disconnect)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            not_connected
    end.

-spec start_stream(xcl:session()) -> ok.
start_stream(#session{pid = Pid, jid = Jid}) ->
    gen_server:cast(Pid, {start_stream, Jid#jid.domain}).

-spec end_stream(xcl:session()) -> ok.
end_stream(#session{pid = Pid}) ->
    gen_server:cast(Pid, end_stream).

-spec send_stanza(xcl:session(), xmlstreamelement()) -> ok.
send_stanza(#session{pid = Pid}, El) ->
    gen_server:cast(Pid, {send, exml:to_iolist(El)}).

-spec enable_tls(xcl:session()) -> any().
enable_tls(_Session) ->
    throw({tls_not_supported}).

-spec reset_parser(xcl:session()) -> ok.
reset_parser(#session{pid = Pid}) ->
    gen_server:cast(Pid, reset_parser).

-spec is_connected(xcl:session()) -> boolean().
is_connected(#session{pid = Pid}) ->
    erlang:is_process_alive(Pid).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Client]) ->
    {ok, #state{client = Client}}.

handle_call({connect, Args}, _From, State) ->
    Host = to_list(proplists:get_value(host, Args)),
    Port = to_integer(proplists:get_value(port, Args)),
    Path = to_list(proplists:get_value(path, Args)),
    LegacyWS = proplists:get_value(legacy_ws, Args, false),
    Opts = [],
    xcl_log:debug("[xcl_ws] Connect websocket ~s:~p~s with options: ~p",
         [Host, Port, Path, Opts]),
    try
        {ok, WS} = wsecli:start(Host, Port, Path, Opts),
        Pid = self(),
        wsecli:on_open(WS, fun() -> Pid ! opened end),
        wsecli:on_error(WS, fun(Reason) -> Pid ! {error, Reason} end),
        wsecli:on_message(WS, fun(Type, Data) -> Pid ! {Type, Data} end),
        wsecli:on_close(WS, fun(_) -> Pid ! tcp_closed end),
        wait_for_ws_open(),
        Parser = create_parser(LegacyWS),
        Session = #session{transport = ?MODULE,
                           pid = Pid},
        {reply, {ok, Session}, State#state{ws = WS,
                                           parser = Parser,
                                           legacy_ws = LegacyWS}}
    catch
        _:Reason ->
            xcl_log:error("Failed websocket connection: ~p", [Reason]),
            {stop, normal, {error, Reason}, State}
    end;
handle_call(disconnect, _From, State) ->
    {stop, normal, ok, State};
handle_call(Event, _From, State) ->
    xcl_log:warning("[xcl_ws] Unhandled call: ~p", [Event]),
    {noreply, State}.

handle_cast({send, Data}, #state{ws = WS} = State) ->
    send_data(WS, Data),
    {noreply, State};
handle_cast({start_stream, Domain}, #state{legacy_ws = LegacyWS,
                                           ws = WS} = State) ->
    case LegacyWS of
        true  -> send_xml(WS, xcl_stanza:stream_start(Domain, client));
        false -> send_xml(WS, xcl_stanza:ws_open(Domain))
    end,
    {noreply, State};
handle_cast(end_stream, #state{legacy_ws = LegacyWS,
                               ws = WS} = State) ->
    case LegacyWS of
        true  -> send_xml(WS, xcl_stanza:stream_end());
        false -> send_xml(WS, xcl_stanza:ws_close())
    end,
    {noreply, State};
handle_cast(reset_parser, #state{parser = Parser} = State) ->
    {ok, Parser1} = exml_stream:reset_parser(Parser),
    {noreply, State#state{parser = Parser1}};
handle_cast(Event, State) ->
    xcl_log:warning("[xcl_ws] Unhandled cast: ~p", [Event]),
    {noreply, State}.

handle_info({text, Data}, State) ->
    handle_info({binary, list_to_binary(lists:flatten(Data))}, State);
handle_info({binary, Data}, State) ->
    handle_data(Data, State);
handle_info({error, Reason}, State) ->
    {stop, Reason, State};
handle_info(Event, State) ->
    xcl_log:warning("[xcl_ws] Unhandled event: ~p", [Event]),
    {noreply, State}.

terminate(Reason, #state{ws = WS} = State) ->
    xcl_log:debug("[xcl_ws] Terminate reason: ~p", [Reason]),
    free_parser(State),
    wsecli:stop(WS).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Private functions
%%%===================================================================

-spec send_xml(pid(), xmlstreamelement()) -> ok.
send_xml(WS, El) ->
    wsecli:send(WS, exml:to_iolist(El)).

-spec send_data(pid(), iolist()) -> ok.
send_data(WS, Data) ->
    wsecli:send(WS, Data).

-spec create_parser(boolean()) -> exml_stream:parser().
create_parser(true) ->
    create_parser2([]);
create_parser(false) ->
    create_parser2([{infinite_stream, true}, {autoreset, true}]).
create_parser2(Opts) ->
    {ok, Parser} = exml_stream:new_parser(Opts),
    Parser.

-spec wait_for_ws_open() -> ok.
wait_for_ws_open() ->
    receive
        opened ->
            ok
    after ?XCL_WS_HANDSHAKE_TIMEOUT ->
            throw(handshake_timeout)
    end.

-spec handle_data(binary(), #state{}) ->
    {noreply, #state{}} | {stop, normal, #state{}}.
handle_data(Data, #state{parser = Parser} = State) ->
    {ok, Parser1, Stanzas} = exml_stream:parse(Parser, Data),
    process_stanzas(Stanzas, State#state{parser = Parser1}).

-spec process_stanzas([xmlstreamelement()], #state{}) ->
    {noreply, #state{}} | {stop, normal, #state{}}.
process_stanzas([], State) ->
    {noreply, State};
process_stanzas([#xmlstreamend{} = Stanza | _Tail], #state{client = Client} = State) ->
    xcl_log:debug("[xcl_ws] Received stream end, closing websocket"),
    Client ! {stanza, self(), Stanza},
    {stop, normal, State};
process_stanzas([#xmlel{name = <<"stream:error">>} = Stanza | _Tail], State) ->
    stream_error(Stanza, State);
process_stanzas([Stanza | Tail], #state{client = Client} = State) ->
    Client ! {stanza, self(), Stanza},
    process_stanzas(Tail, State).

-spec free_parser(#state{}) -> any().
free_parser(#state{parser = undefined}) ->
    ok;
free_parser(#state{parser = Parser}) ->
    exml_stream:free_parser(Parser).

-spec stream_error(#xmlel{}, #state{}) -> {stop, normal, #state{}}.
stream_error(Stanza, #state{client = Client} = State) ->
    xcl_log:warning("[xcl_ws] Received stream error, closing websocket [~p]", [Stanza]),
    Client ! {stream_error, self(), Stanza},
    {stop, normal, State}.

