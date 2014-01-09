%% Configuration
-define(XCL_RECEIVE_TIMEOUT, 5000).
-define(XCL_TCP_OPTS, [binary,
                       {active, once},
                       {packet, 0},
                       {reuseaddr, false}]).
-define(XCL_TLS_OPTS, [{reuse_sessions, true}]).

%% Default Options
-define(XCL_DEFAULT_AUTH, plain).
-define(XCL_DEFAULT_TRANSPORT, socket).

