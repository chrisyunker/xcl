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

-behaviour(websocket_client_handler).

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

% websocket_client_handler callbacks
-export([init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-import(xcl_util, [to_list/1, to_integer/1]).

-record(state, {client :: pid(),
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
    LegacyWS = proplists:get_value(legacy_ws, Args, false),
    Host = to_list(proplists:get_value(host, Args)),
    Port = to_integer(proplists:get_value(port, Args)),
    Path = to_list(proplists:get_value(path, Args)),
    SSL = case proplists:get_value(tls, Args, false) of
        tls ->
            ssl:start(),
            true;
        _ ->
            false
    end,
    URL = url(SSL, Host, Port, Path),
    xcl_log:debug("[xcl_ws] Connect websocket URL: ~p", [URL]),
    try
        {ok, WS} = websocket_client:start_link(URL,
                                               ?MODULE,
                                               [self(), LegacyWS]),
        Session = #session{transport = ?MODULE,
                           pid = WS},
        {ok, Session}
    catch
        _:Reason ->
            xcl_log:error("Failed websocket connection: ~p", [Reason]),
            {error, Reason}
    end.

-spec disconnect(xcl:session()) -> ok | not_connected.
disconnect(#session{pid = Pid}) ->
    try
        websocket_client:cast(Pid, close)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            not_connected
    end.

-spec start_stream(xcl:session()) -> any().
start_stream(#session{pid = Pid, jid = Jid}) ->
    Pid ! {start_stream, Jid#jid.domain}.

-spec end_stream(xcl:session()) -> any().
end_stream(#session{pid = Pid}) ->
    Pid ! end_stream.

-spec send_stanza(xcl:session(), exml_stream:element()) -> ok.
send_stanza(#session{pid = Pid}, El) ->
    websocket_client:cast(Pid, {text, exml:to_binary(El)}).

-spec enable_tls(xcl:session()) -> any().
enable_tls(_Session) ->
    throw({tls_not_supported}).

-spec reset_parser(xcl:session()) -> any().
reset_parser(#session{pid = Pid}) ->
    Pid ! reset_parser.

-spec is_connected(xcl:session()) -> boolean().
is_connected(#session{pid = Pid}) ->
    erlang:is_process_alive(Pid).

%%%===================================================================
%%% websocket_client_handler callbacks
%%%===================================================================
-spec init(list(), websocket_req:req()) -> {ok, #state{}}.
init([Pid, LegacyWS], _ConnState) ->
    Parser = create_parser(LegacyWS),
    {ok, #state{parser = Parser,
                client = Pid}}.

websocket_handle({binary, Data}, _ConnState, State) ->
    handle_data(Data, State),
    {ok, State};
websocket_handle({text, Data}, _ConnState, State) ->
    handle_data(list_to_binary(lists:flatten(Data)), State),
    {ok, State};
websocket_handle(Msg, _ConnState, State) ->
    xcl_log:info("websocket_handle: unhandled message: ~p", [Msg]),
    {ok, State}.

websocket_info({start_stream, Domain}, _ConnState, #state{legacy_ws = true} = State) ->
    Stanza = xcl_stanza:stream_start(Domain, client),
    websocket_client:cast(self(), {text, exml:to_binary(Stanza)}),
    {noreply, State};
websocket_info({start_stream, Domain}, _ConnState, #state{legacy_ws = false} = State) ->
    Stanza = xcl_stanza:ws_open(Domain),
    websocket_client:cast(self(), {text, exml:to_binary(Stanza)}),
    {noreply, State};
websocket_info(end_stream, _ConnState, #state{legacy_ws = true} = State) ->
    Stanza = xcl_stanza:stream_end(),
    websocket_client:cast(self(), {text, exml:to_binary(Stanza)}),
    {noreply, State};
websocket_info(end_stream, _ConnState, #state{legacy_ws = false} = State) ->
    Stanza = xcl_stanza:ws_close(),
    websocket_client:cast(self(), {text, exml:to_binary(Stanza)}),
    {noreply, State};

websocket_info(reset_parser, _ConnState, #state{parser = Parser} = State) ->
    {ok, Parser1} = exml_stream:reset_parser(Parser),
    {noreply, State#state{parser = Parser1}};
websocket_info(stop, _ConnState, State) ->
    {close, <<>>, State};
websocket_info(Type, _ConnState, State) ->
    xcl_log:info("websocket_info: unhandled type: ~p", [Type]),
    {reply, <<>>, State}.

websocket_terminate(Reason, _Req, State) ->
    xcl_log:debug("[xcl_ws] Terminate reason: ~p", [Reason]),
    free_parser(State),
    case is_process_alive(State#state.client) of
        true -> gen_server:cast(State#state.client, ws_closed);
        false -> ok
    end,
    ok.

%%%===================================================================
%%% Private functions
%%%===================================================================
-spec create_parser(boolean()) -> exml_stream:parser().
create_parser(true) ->
    create_parser2([]);
create_parser(false) ->
    create_parser2([{infinite_stream, true}, {autoreset, true}]).
create_parser2(Opts) ->
    {ok, Parser} = exml_stream:new_parser(Opts),
    Parser.

-spec handle_data(binary(), #state{}) ->
    {noreply, #state{}} | {stop, normal, #state{}}.
handle_data(Data, #state{parser = Parser} = State) ->
    {ok, Parser1, Stanzas} = exml_stream:parse(Parser, Data),
    process_stanzas(Stanzas, State#state{parser = Parser1}).

-spec process_stanzas([exml_stream:element()], #state{}) ->
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

-spec stream_error(exml:element(), #state{}) -> {stop, normal, #state{}}.
stream_error(Stanza, #state{client = Client} = State) ->
    xcl_log:warning("[xcl_ws] Received stream error, closing websocket [~p]", [Stanza]),
    Client ! {stream_error, self(), Stanza},
    {stop, normal, State}.

-spec url(boolean(), string(), integer(), string()) -> string().
url(SSL, Host, Port, Path) ->
    Scheme = case SSL of
                 true -> "wss://";
                 false -> "ws://"
             end,
    Scheme ++ Host ++ ":" ++ to_list(Port) ++ "/" ++ Path.


