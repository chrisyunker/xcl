%% Records

-record(session, {transport :: atom(),
                  pid :: pid(),
                  socket :: term(),
                  jid :: xcl:jid(),
                  compress = none :: atom(),
                  tls = false :: boolean()}).

-record(jid, {node :: binary(),
              domain :: binary(),
              resource = <<>> :: binary()}).

%% XMPP namespaces

%% RFC 6120: XMPP Core
-define(NS_XMPP, <<"http://etherx.jabber.org/streams">>).

-define(NS_TLS, <<"urn:ietf:params:xml:ns:xmpp-tls">>).

-define(NS_SASL, <<"urn:ietf:params:xml:ns:xmpp-sasl">>).
-define(NS_BIND, <<"urn:ietf:params:xml:ns:xmpp-bind">>).

-define(NS_JABBER_CLIENT, <<"jabber:client">>).
-define(NS_JABBER_SERVER, <<"jabber:server">>).

-define(NS_ROSTER, <<"jabber:iq:roster">>).

%% RFC 6121: XMPP IM
-define(NS_SESSION, <<"urn:ietf:params:xml:ns:xmpp-session">>).

%% XEP-0016: Privacy Lists
-define(NS_PRIVACY, <<"jabber:iq:privacy">>).

%% XEP-0030: Service Discovery
-define(NS_DISCO_INFO, <<"http://jabber.org/protocol/disco#info">>).
-define(NS_DISCO_ITEMS, <<"http://jabber.org/protocol/disco#items">>).

%% XEP-0045: Multi-User Chat
-define(NS_MUC, <<"http://jabber.org/protocol/muc">>).
-define(NS_MUC_USER, <<"http://jabber.org/protocol/muc#user">>).

%% XEP-0091: Legacy Delayed Delivery
-define(NS_DELAY, <<"jabber:x:delay">>).

%% XEP-0128: Service Discovery Extensions
-define(VAR_MUC_OCCUPANTS, <<"muc#roominfo_occupants">>).

%% XEP-0138: Stream Compression
-define(NS_COMPRESS, <<"http://jabber.org/protocol/compress">>).
-define(NS_FEATURE_COMPRESS, <<"http://jabber.org/features/compress">>).

%% draft-ietf-xmpp-websocket-07
-define(NS_FRAMING, <<"urn:ietf:params:xml:ns:xmpp-framing">>).
