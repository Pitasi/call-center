-module(operator_tests).

-include_lib("eunit/include/eunit.hrl").

operator_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun (Pid) ->
         [ask(Pid)]
     end}.

start() ->
    %% spawn an operator that doesn't "expire"
    {ok, Pid} = operator:start_link({infinity, 99999}),
    Pid.

stop(Pid) -> operator:shutdown(Pid).

ask(Pid) ->
    Questions = [
        "testquestion",
        "123lkadsf alskjflas saf",
        "",
        "1 2 3 a b c   ",
        "--::Adfaaa!@$%^#  23545 #$%@#%"
    ],
    [
        ?_assertEqual(Question, operator:ask(Pid, Question)) || %% operator echoes question
        Question <- Questions
    ].


operator_timeout_test() ->
    {ok, Pid} = operator:start({10, 99999}),
    Ref = erlang:monitor(process, Pid),
    Result = receive
        {'DOWN', Ref, _, Pid, _} -> ok
    after 100 -> {error, operator_didnt_quit}
    end,
    ?assertEqual(ok, Result).

operator_max_req_test() ->
    MaxReq = 10,
    {ok, Pid} = operator:start({infinity, MaxReq}),
    Ref = erlang:monitor(process, Pid),
    [operator:ask(Pid, "question") || _ <- lists:seq(1, MaxReq)],
    Result = receive
        {'DOWN', Ref, _, Pid, _} -> ok
    after 0 -> {error, operator_didnt_quit}
    end,
    ?assertEqual(ok, Result).
