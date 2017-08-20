%%%-------------------------------------------------------------------
%%% @author Chris Yunker <chris@yunker.io>
%%% @copyright (C) 2014, Chris Yunker
%%% @doc Create and extract XMPP stanzas
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xcl_xml).

-include_lib("exml/include/exml_stream.hrl").
-include("xcl.hrl").

-export([attr/2,
         el/1, el/3,
         el_simple/2,
         cdata/1,
         xml_to_list/1,
         xml_to_iolist/1]).

-export([is_element/1,
         get_name/1,
         get_xmlns/1,
         get_attr/2, get_attr/3,
         set_attr/2,
         remove_attr/2,
         set_cdata/2,
         get_cdata/1,
         get_subelement/2,
         get_subelements/1,
         has_subelement/2,
         replace_subelement/2,
         replace_child/3,
         get_children/1,
         set_children/2,
         add_children/2,
         get_path/2, get_path/3]).

%%====================================================================
%% API functions
%%====================================================================
-spec attr(binary(), binary()) -> exml:attr().
attr(Name, Value) ->
    {Name, exml:escape_attr(Value)}.

-spec el(binary()) -> exml:element().
el(Name) ->
    #xmlel{name = Name}.

-spec el(binary(), [exml:attr()],
         [exml:element() | exml:cdata()]) -> exml:element().
el(Name, Attrs, Children) ->
    #xmlel{name = Name,
           attrs = Attrs,
           children = Children}.

-spec el_simple(binary() | string(), iodata()) -> exml:element().
el_simple(Name, Content) ->
    el(Name, [], [exml:escape_cdata(Content)]).

-spec cdata(iodata()) -> exml:cdata().
cdata(Content) ->
    exml:escape_cdata(Content).

-spec xml_to_list(exml:item()) -> string().
xml_to_list(Doc) ->
    exml:to_list(Doc).

-spec xml_to_iolist(exml:item() | [exml:item()]) -> iolist().
xml_to_iolist(Doc) ->
    exml:to_iolist(Doc).


-spec is_element(exml:element()) -> boolean().
is_element(#xmlel{}) -> true;
is_element(_)        -> false.

-spec get_name(exml:element()) -> binary().
get_name(#xmlel{name = Name}) ->
    Name.

-spec get_xmlns(exml:element()) -> binary() | undefined.
get_xmlns(El) ->
    get_attr(El, <<"xmlns">>).

-spec get_attr(exml:element(), binary()) -> binary() | undefined.
get_attr(El, Name) ->
    exml_query:attr(El, Name, undefined).

-spec get_attr(exml:element(), binary(), any()) -> binary() | undefined.
get_attr(El, Name, Default) ->
    exml_query:attr(El, Name, Default).

-spec set_attr(exml:element(), exml:attr()) -> exml:element().
set_attr(El, Attr) ->
    El#xmlel{attrs = lists:keystore(element(1, Attr), 1, El#xmlel.attrs, Attr)}.

-spec remove_attr(exml:element(), binary()) -> exml:element().
remove_attr(El, Name) ->
    El#xmlel{attrs = lists:keydelete(Name, 1, El#xmlel.attrs)}.

-spec set_cdata(exml:element(), binary()) -> exml:element().
set_cdata(El, Content) ->
    El#xmlel{children = [exml:escape_cdata(Content)]}.

-spec get_cdata(exml:element()) -> binary().
get_cdata(El) ->
    exml_query:cdata(El).

-spec get_subelement(exml:element(), binary()) -> exml:element() | undefined.
get_subelement(El, Name) ->
    exml_query:subelement(El, Name, undefined).

-spec get_subelements(exml:element()) -> [exml:element()].
get_subelements(#xmlel{children = Children}) ->
    lists:filter(fun is_element/1, Children).

-spec has_subelement(exml:element(), binary()) -> boolean().
has_subelement(El, Name) ->
    get_subelement(El, Name) /= undefined.

-spec replace_subelement(exml:element(), exml:element()) -> exml:element().
replace_subelement(#xmlel{children = Children} = El,
                   #xmlel{name = Name} = SubEl) ->
    El#xmlel{children = lists:keyreplace(Name, 1, Children, SubEl)}.

-spec get_children(exml:element()) -> [exml:element()].
get_children(#xmlel{children = Children}) ->
    Children.

-spec set_children(exml:element(), [exml:element()]) -> exml:element().
set_children(El, Children) ->
    El#xmlel{children = Children}.

-spec add_children(exml:element(), exml:element() | [exml:element()]) ->
    exml:element().
add_children(El, SubEls) when is_list(SubEls) ->
    El#xmlel{children = SubEls ++ El#xmlel.children};
add_children(El, SubEl) ->
    add_children(El, [SubEl]).

-spec replace_child(exml:element(), exml:element(), exml:element()) ->
    exml:element().
replace_child(#xmlel{children = Children} = El, OldChild, NewChild) ->
    Children2 = lists:foldl(
            fun(C, Acc) when C =:= OldChild ->
                    [NewChild | Acc];
               (C, Acc) ->
                    [C | Acc]
            end, [], Children),
    El#xmlel{children = Children2}.

-spec get_path(exml:element(), exml_query:path()) ->
    exml:element() | binary() | undefined.
get_path(Element, Path) ->
    get_path(Element, Path, undefined).

-spec get_path(exml:element(), exml_query:path(), Other) ->
    exml:element() | binary() | Other.
get_path(Element, Path, Default) ->
    exml_query:path(Element, Path, Default).

