-module(jokes_tests).

-include_lib("eunit/include/eunit.hrl").

jokes_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun (_) ->
         [of_today_remain_constant()]
     end}.

start() -> jokes:start_link().

stop(_) -> jokes:shutdown().

of_today_remain_constant() ->
    Joke1 = jokes:of_today(),
    Joke2 = jokes:of_today(),
    ?_assertMatch(true, Joke1 =:= Joke2).
