Overview
--------

**xcl** is a client XMPP library implemented in Erlang.

Building
--------

**xcl** is a rebar-compatible OTP application. It depends on the [exml](https://github.com/esl/exml) and [lager](https://github.com/basho/lager) libraries.

To build run *make* or use rebar tool.

Usage
-----

Create a properly list with the following configured arguments:

```erlang
Args = [{username, "username"},
        {password, "password1"},
        {domain, "yunker.io"},
        {resource, "xcl"},
        {host, "yunker.io"},
        {port, 5223},
        {transport, socket | ws},
        {auth, plain},
        {tls, none | tls | starttls},
        {compress, none}]
```

Start session:

```erlang
{ok, Session} = xcl_session:start(Args)
```

Send a stanza:

```erlang
ok = xcl_session:send_stanza(Session, Xmlel)
```

Receive a stanza:

```erlang
{ok, Xmlel} = xcl_session:receive_stanza(Session)
```

Check if session is valid:

```erlang
true | false = xcl_session:is_valid(Session)
```

Stop session:

```erlang
ok = xcl_session:stop(Session)
```

