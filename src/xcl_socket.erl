%%%-------------------------------------------------------------------
%%% @author Chris Yunker <chris@yunker.io>
%%% @copyright (C) 2014, Chris Yunker
%%% @doc Socket transport implementation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xcl_socket).

-include_lib("exml/include/exml_stream.hrl").
-include("xcl.hrl").
-include("xcl_config.hrl").

-behaviour(gen_server).

-export([connect/1,
         disconnect/1,
         send_stanza/2,
         enable_tls/1,
         reset_parser/1,
         is_connected/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-import(xcl_util, [to_list/1, to_integer/1]).

-record(state, {module :: atom(),
                client :: pid(),
                socket :: term(),
                parser :: tuple(),
                compress = none :: atom()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec connect([tuple()]) -> {ok, xcl:session()}.
connect(Args) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [self()], []),
    Session = gen_server:call(Pid, {connect, Args}),
    {ok, Session}.

-spec disconnect(xcl:session()) -> ok | not_connected.
disconnect(#session{pid = Pid}) ->
    try
        gen_server:call(Pid, disconnect)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            not_connected
    end.

-spec send_stanza(xcl:session(), xmlstreamelement()) -> ok | {error, term()}.
send_stanza(#session{socket = Socket, tls = true}, El) ->
    ssl:send(Socket, exml:to_iolist(El));
send_stanza(#session{socket = Socket}, El) ->
    gen_tcp:send(Socket, exml:to_iolist(El)).

-spec enable_tls(xcl:session()) -> xcl:session().
enable_tls(#session{pid = Pid} = Session) ->
    gen_server:call(Pid, {enable_tls, Session}).

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
    {Module, Opts, Tls} = case proplists:get_value(tls, Args) of
        tls ->
            ssl:start(),
            {ssl, ?XCL_TLS_OPTS ++ ?XCL_TCP_OPTS, true};
        _ ->
            {gen_tcp, ?XCL_TCP_OPTS, false}
    end,
    xcl_log:debug("[xcl_socket] Connect socket ~s:~p with options: ~p",
         [Host, Port, Opts]),
    {ok, Socket} = Module:connect(Host, Port, Opts),
    {ok, Parser} = exml_stream:new_parser(),
    Session = #session{transport = ?MODULE,
                       pid = self(),
                       socket = Socket,
                       tls = Tls},
    {reply, Session, State#state{module = Module,
                                 socket = Socket,
                                 parser = Parser}};
handle_call({enable_tls, Session}, _From, #state{socket = Socket} = State) ->
    ssl:start(),
    xcl_log:debug("[xcl_socket] Enabling TLS with options: ~p", [?XCL_TLS_OPTS]),
    {ok, Socket1} = ssl:connect(Socket, ?XCL_TLS_OPTS),
    {ok, Parser} = exml_stream:new_parser(),
    Session1 = Session#session{socket = Socket1,
                               tls = true},
    {reply, Session1, State#state{module = ssl,
                                  socket = Socket1,
                                  parser = Parser}};
handle_call(disconnect, _From, State) ->
    {stop, normal, ok, State};
handle_call(Event, _From, State) ->
    xcl_log:warning("[xcl_socket] Unhandled call: ~p", [Event]),
    {noreply, State}.

handle_cast(reset_parser, #state{parser = Parser} = State) ->
    {ok, Parser1} = exml_stream:reset_parser(Parser),
    {noreply, State#state{parser = Parser1}};
handle_cast(Event, State) ->
    xcl_log:warning("[xcl_socket] Unhandled cast: ~p", [Event]),
    {noreply, State}.

handle_info({tcp, Socket, Data}, #state{socket = Socket} = State) ->
    receive_data(Data, State);
handle_info({ssl, Socket, Data}, #state{socket = Socket} = State) ->
    receive_data(Data, State);
handle_info({tcp_error, Socket, Reason}, #state{socket = Socket} = State) ->
    socket_error(tcp_error, Reason, State);
handle_info({ssl_error, Socket, Reason}, #state{socket = Socket} = State) ->
    socket_error(ssl_error, Reason, State);
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    socket_closed(tcp_closed, State);
handle_info({ssl_closed, Socket}, #state{socket = Socket} = State) ->
    socket_closed(ssl_closed, State);
handle_info(Event, State) ->
    xcl_log:warning("[xcl_socket] Unhandled event: ~p", [Event]),
    {noreply, State}.

terminate(Reason, State) ->
    xcl_log:debug("[xcl_socket] Terminate reason: ~p", [Reason]),
    free_parser(State),
    free_socket(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Private functions
%%%===================================================================

-spec receive_data(binary(), #state{}) ->
    {noreply, #state{}} | {stop, normal, #state{}}.
receive_data(Data, #state{client = Client,
                          parser = Parser,
                          module = Module,
                          socket = Socket} = State) ->
    setopts(Module, Socket, [{active, once}]),
    {ok, Parser1, Stanzas} = exml_stream:parse(Parser, Data),
    State1 = State#state{parser = Parser1},
    Action = process_stanza(continue, Client, Stanzas),
    case Action of
        continue -> {noreply, State1};
        stop -> {stop, normal, State1}
    end.

-spec process_stanza(atom(), pid(), [xmlstreamelement()]) -> atom().
process_stanza(Action, _Client, []) ->
    Action;
process_stanza(_Action, _Client, [#xmlstreamend{} | _Tail]) ->
    xcl_log:debug("[xcl_socket] Received stream end, closing socket"),
    stop;
process_stanza(Action, Client, [Stanza | Tail]) ->
    Client ! {stanza, self(), Stanza},
    process_stanza(Action, Client, Tail).

-spec free_socket(#state{}) -> any().
free_socket(#state{socket = undefined}) ->
    ok;
free_socket(#state{socket = Socket, module = Module}) ->
    Module:close(Socket).

-spec free_parser(#state{}) -> any().
free_parser(#state{parser = undefined}) ->
    ok;
free_parser(#state{parser = Parser}) ->
    exml_stream:free_parser(Parser).

-spec socket_error(atom(), atom(), #state{}) -> {noreply, #state{}}.
socket_error(Event, Reason, #state{module = Module, socket = Socket} = State) ->
    xcl_log:warning("[xcl_socket] Received socket error: ~p, reason: ~p",
         [Event, Reason]),
    setopts(Module, Socket, [{active, once}]),
    {noreply, State}.

-spec socket_closed(atom(), #state{}) -> {stop, normal, #state{}}.
socket_closed(Event, #state{client = Client} = State) ->
    xcl_log:debug("[xcl_socket] Received socket closed: ~p", [Event]),
    Client ! {Event, self()},
    {stop, normal, State}.

-spec setopts(atom(), term(), [term()]) -> ok | {error, atom()}.
setopts(gen_tcp, Socket, Opts) ->
    inet:setopts(Socket, Opts);
setopts(Module, Socket, Opts) ->
    Module:setopts(Socket, Opts).

