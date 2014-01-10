%% Configuration
-define(XCL_RECV_TIMEOUT, 5000).
-define(XCL_TCP_OPTS, [binary,
                       {active, once},
                       {packet, 0},
                       {reuseaddr, false}]).
-define(XCL_TLS_OPTS, [{reuse_sessions, true}]).

-define(XCL_TRANS_CONN_TIMEOUT, 6000).
-define(XCL_SOCK_CONN_TIMEOUT, 5000).

%% Default Options
-define(XCL_DEFAULT_AUTH, plain).
-define(XCL_DEFAULT_TRANS, socket).

