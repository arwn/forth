-module(forth).
-export([run/1, run_file/1]).

-record(state, {stack, words, immediates, compiling, compile_accumulator}).

run(Program) ->
    S = #state{
        stack = [],
        words = builtins(),
        immediates = sets:from_list([";"]),
        compiling = false,
        compile_accumulator = []
    },
    run(parse(Program), S),
    ok.

run_file(Filename) ->
    {ok, File} = file:read_file(Filename),
    Program = re:replace(binary_to_list(File), "\n", " ", [global, {return, list}]),
    run(Program).

%% Actual Code ^TM

looks_like(String, number) ->
    looks_like(String, "^[0-9]+$");
looks_like(String, bool) ->
    looks_like(String, "^(true|false)$");
looks_like(String, RegEx) ->
    re:run(String, RegEx) =/= nomatch.

is_immediate(X, S) ->
    sets:is_element(X, S#state.immediates).

make_compiled_function(Fdefinition) ->
    fun(Program, S) ->
        {_CompiledProgram, NewState} = run(Fdefinition, S),
        {Program, NewState}
    end.

compile(Program, S, "immediate") ->
    Fname = lists:last(S#state.compile_accumulator),
    NewState = S#state{
        immediates = sets:add_element(
            Fname, S#state.immediates
        )
    },
    compile(tl(Program), NewState, false);
compile(Program, S, _NextWord) ->
    [Fname | Fdef] = lists:reverse(S#state.compile_accumulator),
    NewState = S#state{
        words = (maps:put(Fname, make_compiled_function(Fdef), S#state.words)),
        compiling = false
    },
    {Program, NewState}.

builtins() ->
    #{
        "print" => fun(Program, S) ->
            [Head | _] = S#state.stack,
            io:format("~p\n", [Head]),
            {Program, S}
        end,

        "+" => fun(Program, S) ->
            [Lhs, Rhs | Rest] = S#state.stack,
            NewStack = [Lhs + Rhs | Rest],
            {Program, S#state{stack = NewStack}}
        end,

        "branch?" => fun(Program, S) ->
            [Cond | Stack] = S#state.stack,
            [Option1, Option2 | NewProgram] = Program,
            NewProgram1 =
                if
                    Cond -> [Option1 | NewProgram];
                    true -> [Option2 | NewProgram]
                end,
            {NewProgram1, S#state{stack = Stack}}
        end,

        ":" => fun(Program, S) ->
            {Program, S#state{compiling = true, compile_accumulator = []}}
        end,

        ";" => fun(Program, S) ->
            [NextWord | _] = Program,
            compile(Program, S, NextWord)
        end,

        "nop" => fun(Program, S) ->
            {Program, S}
        end
    }.

parse(Program) ->
    F = fun(String) ->
        IsNum = looks_like(String, number),
        IsBool = looks_like(String, bool),
        if
            IsNum ->
                list_to_integer(String);
            IsBool ->
                if
                    String == "false" -> false;
                    true -> true
                end;
            true ->
                String
        end
    end,
    Split = string:split(Program, " ", all),
    lists:map(F, Split).

exec_word(Word, Rest, S) ->
    F = maps:get(Word, S#state.words),
    case S#state.compiling of
        false ->
            F(Rest, S);
        true ->
            {_, NewState} = F(Rest, S#state{compiling = false}),
            F(Rest, NewState#state{compiling = S#state.compiling})
    end.

run([], S) ->
    {[], S};
run(Program, S) ->
    % io:format("Executing program~p with stack~p, acc~p\n", [
    %     Program, S#state.stack, S#state.compile_accumulator
    % ]),
    [Word | Rest] = Program,
    IsWord = maps:is_key(Word, S#state.words),
    IsImmediate = is_immediate(Word, S),
    if
        S#state.compiling and not IsImmediate ->
            {NewProgram, NewState} = {tl(Program), S#state{
                compile_accumulator = [Word | S#state.compile_accumulator],
                compiling = true
            }};
        IsWord ->
            {NewProgram, NewState} = exec_word(Word, Rest, S);
        true ->
            NewStack = [Word | S#state.stack],
            NewState = S#state{stack = NewStack},
            NewProgram = Rest
    end,
    run(NewProgram, NewState).
