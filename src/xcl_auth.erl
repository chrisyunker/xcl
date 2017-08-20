%%%-------------------------------------------------------------------
%%% @author Chris Yunker <chris@yunker.io>
%%% @copyright (C) 2014, Chris Yunker
%%% @doc XMPP Authentication functions
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xcl_auth).

-include("xcl.hrl").
-include("xcl_config.hrl").

-export([authenticate/2]).

%%====================================================================
%% API functions
%%====================================================================
-spec authenticate(xcl:session(), list()) -> ok.
authenticate(Session, Args) ->
    Type = proplists:get_value(auth, Args, ?XCL_DEFAULT_AUTH),
    case Type of
        plain -> auth_plain(Session, Args);
        _ ->
            throw({fail_authentication, auth_type_not_supported})
    end.

-spec auth_plain(xcl:session(), list()) -> ok.
auth_plain(#session{transport = Trans} = Session, Args) ->
    Username = proplists:get_value(username, Args),
    Password = proplists:get_value(password, Args),
    Stanza = xcl_stanza:auth_plain(Username, Password),
    ok = Trans:send_stanza(Session, Stanza),
    {ok, AuthEl} = xcl_session:receive_stanza(Session, auth_reply),
    case xcl_stanza:verify_auth(AuthEl) of
        true -> ok;
        false -> throw({fail_authentication, bad_credentials})
    end.

