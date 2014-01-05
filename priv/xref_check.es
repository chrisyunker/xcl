#!/usr/bin/env escript

main(_) ->
    {ok, _} = xref:start(xref),
    xref:set_library_path(xref, code_path() ++ ["ebin" | deps()]),
    xref:set_default(xref, [{warnings, true}]),
    xref:add_directory(xref, "ebin"),
    code:add_path("ebin"),

    Ignored = ignored(),
    {ok, UndefinedCalls0} = xref:analyze(xref, undefined_function_calls),
    UndefinedCalls =
        [{find_mfa_source(Caller), format_fa(Caller), format_mfa(Target)}
         || {Caller, Target} <- UndefinedCalls0,
            not lists:member(Target, Ignored)],

    lists:foreach(
      fun({{Source, Line}, FunStr, Target}) ->
              io:format("~s:~w: Warning ~s calls undefined function ~s\n",
                        [Source, Line, FunStr, Target])
      end, UndefinedCalls).

code_path() ->
    [P || P <- code:get_path(), filelib:is_dir(P)].

format_mfa({M, F, A}) ->
    io_lib:format("~s:~s/~w", [M, F, A]).

format_fa({_M, F, A}) ->
    io_lib:format("~s/~w", [F, A]).

find_mfa_source({M, F, A}) ->
    {M, Bin, _} = code:get_object_code(M),
    AbstractCode = beam_lib:chunks(Bin, [abstract_code]),
    {ok, {M, [{abstract_code, {raw_abstract_v1, Code}}]}} = AbstractCode,
    %% Extract the original source filename from the abstract code
    [{attribute, 1, file, {Source, _}} | _] = Code,
    %% Extract the line number for a given function def
    Fn = [E || E <- Code,
               safe_element(1, E) == function,
               safe_element(3, E) == F,
               safe_element(4, E) == A],
    case Fn of
        [{function, Line, F, _, _}] -> {Source, Line};
        %% do not crash if functions are exported, even though they
        %% are not in the source.
        %% parameterized modules add new/1 and instance/1 for example.
        [] -> {Source, function_not_found}
    end.

safe_element(N, Tuple) ->
    case catch(element(N, Tuple)) of
        {'EXIT', {badarg, _}} ->
            undefined;
        Value ->
            Value
    end.

ignored() ->
    [].

deps() ->
    {ok, Dirs} = file:list_dir("deps"),
    [filename:join(["deps", Dir, "ebin"])
     || Dir <- Dirs,  filelib:is_dir(filename:join("deps", Dir))].
