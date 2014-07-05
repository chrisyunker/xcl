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
         xml_to_binary/1,
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

-import(xcl_util, [to_binary/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec attr(binary() | string(), binary() | string()) -> xmlattr().
attr(Name, Value) ->
    {to_binary(Name), exml:escape_attr(to_binary(Value))}.

-spec el(binary() | string()) -> #xmlel{}.
el(Name) ->
    #xmlel{name = to_binary(Name)}.

-spec el(binary() | string(), [xmlattr()], [#xmlel{} | #xmlcdata{}]) -> #xmlel{}.
el(Name, Attrs, Children) ->
    #xmlel{name = to_binary(Name),
           attrs = Attrs,
           children = Children}.

-spec el_simple(binary() | string(), iodata()) -> #xmlel{}.
el_simple(Name, Content) ->
    el(Name, [], [exml:escape_cdata(Content)]).

-spec cdata(binary()) -> xmlterm().
cdata(Content) ->
    exml:escape_cdata(Content).

-spec xml_to_binary(xmlterm() | [xmlterm()]) -> binary().
xml_to_binary(Doc) ->
    exml:to_binary(Doc).

-spec xml_to_list(xmlterm()) -> string().
xml_to_list(Doc) ->
    exml:to_list(Doc).

-spec xml_to_iolist(xmlterm() | [xmlterm()]) -> iolist().
xml_to_iolist(Doc) ->
    exml:to_iolist(Doc).


-spec is_element(#xmlel{}) -> boolean().
is_element(#xmlel{}) -> true;
is_element(_)        -> false.

-spec get_name(#xmlel{}) -> binary().
get_name(#xmlel{name = Name}) ->
    Name.

-spec get_xmlns(#xmlel{}) -> binary() | undefined.
get_xmlns(El) ->
    get_attr(El, <<"xmlns">>).

-spec get_attr(#xmlel{}, binary()) -> binary() | undefined.
get_attr(El, Name) ->
    exml_query:attr(El, Name, undefined).

-spec get_attr(#xmlel{}, binary(), any()) -> binary() | undefined.
get_attr(El, Name, Default) ->
    exml_query:attr(El, Name, Default).

-spec set_attr(#xmlel{}, xmlattr()) -> #xmlel{}.
set_attr(El, Attr) ->
    El#xmlel{attrs = lists:keystore(element(1, Attr), 1, El#xmlel.attrs, Attr)}.

-spec remove_attr(#xmlel{}, binary()) -> #xmlel{}.
remove_attr(El, Name) ->
    El#xmlel{attrs = lists:keydelete(Name, 1, El#xmlel.attrs)}.

-spec set_cdata(#xmlel{}, binary()) -> #xmlel{}.
set_cdata(El, Content) ->
    El#xmlel{children = [exml:escape_cdata(Content)]}.

-spec get_cdata(#xmlel{}) -> binary().
get_cdata(El) ->
    exml_query:cdata(El).

-spec get_subelement(#xmlel{}, binary()) -> #xmlel{} | undefined.
get_subelement(El, Name) ->
    exml_query:subelement(El, Name, undefined).

-spec get_subelements(#xmlel{}) -> [#xmlel{}].
get_subelements(#xmlel{children = Children}) ->
    lists:filter(fun is_element/1, Children).

-spec has_subelement(#xmlel{}, binary()) -> boolean().
has_subelement(El, Name) ->
    get_subelement(El, Name) /= undefined.

-spec replace_subelement(#xmlel{}, #xmlel{}) -> #xmlel{}.
replace_subelement(#xmlel{children = Children} = El, #xmlel{name = Name} = SubEl) ->
    El#xmlel{children = lists:keyreplace(Name, 1, Children, SubEl)}.

-spec get_children(#xmlel{}) -> [#xmlel{}].
get_children(#xmlel{children = Children}) ->
    Children.

-spec set_children(#xmlel{}, [#xmlel{}]) -> #xmlel{}.
set_children(El, Children) ->
    El#xmlel{children = Children}.

-spec add_children(#xmlel{}, #xmlel{} | [#xmlel{}]) -> #xmlel{}.
add_children(El, SubEls) when is_list(SubEls) ->
    El#xmlel{children = SubEls ++ El#xmlel.children};
add_children(El, SubEl) ->
    add_children(El, [SubEl]).

-spec replace_child(#xmlel{}, #xmlel{}, #xmlel{}) -> #xmlel{}.
replace_child(#xmlel{children = Children} = El, OldChild, NewChild) ->
    Children2 = lists:foldl(
            fun(C, Acc) when C =:= OldChild ->
                    [NewChild | Acc];
               (C, Acc) ->
                    [C | Acc]
            end, [], Children),
    El#xmlel{children = Children2}.

-spec get_path(#xmlel{}, exml_query:path()) ->
    #xmlel{} | binary() | undefined.
get_path(Element, Path) ->
    get_path(Element, Path, undefined).

-spec get_path(#xmlel{}, exml_query:path(), Other) ->
    #xmlel{} | binary() | Other.
get_path(Element, Path, Default) ->
    exml_query:path(Element, Path, Default).

