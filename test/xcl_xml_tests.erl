-module(xcl_xml_tests).

-include_lib("eunit/include/eunit.hrl").
-include("xcl.hrl").

-compile(export_all).


%%--------------------------------------------------------------------
%% tests
%%--------------------------------------------------------------------
attr_test() ->
    OrangeAttr = {<<"type">>, <<"orange">>},
    AppleAttr = {<<"type">>, <<"apple">>},

    AppleEl = xcl_xml:el(<<"test">>, [AppleAttr], []),
    OrangeEl = xcl_xml:el(<<"test">>, [OrangeAttr], []),

    ?assertNotEqual(AppleEl, OrangeEl),

    OrangeEl2 = xcl_xml:set_attr(AppleEl, OrangeAttr),

    ?assertEqual(<<"orange">>, xcl_xml:get_attr(OrangeEl2, <<"type">>)),

    ?assertEqual(OrangeEl, OrangeEl2),

    NoneEl = xcl_xml:remove_attr(AppleEl, <<"type">>),

    ?assertEqual(undefined, xcl_xml:get_attr(NoneEl, <<"type">>)).

