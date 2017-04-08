-module(xcl_transport).

-callback check_args(Args :: list()) -> ok.
-callback connect(Args :: list()) -> {ok, xcl:session()} | {error, term()}.
-callback disconnect(Session :: xcl:session()) -> ok | not_connected.
-callback start_stream(Session :: xcl:session()) -> any().
-callback end_stream(Session :: xcl:session()) -> any().
-callback send_stanza(Session :: xcl:session(), exml_stream:element()) ->
    ok | {error, term()}.
-callback enable_tls(Session :: xcl:session()) -> any().
-callback reset_parser(Session :: xcl:session()) -> any().
-callback is_connected(Session :: xcl:session()) -> boolean().

