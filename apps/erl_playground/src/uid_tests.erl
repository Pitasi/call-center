-module(uid_tests).

-include_lib("eunit/include/eunit.hrl").

generate_min_length_test() ->
    ?assertMatch(true, string:length(uid:generate()) > 1).

generate_different_test() ->
    ID1 = uid:generate(),
    ID2 = uid:generate(),
    ?assertMatch(true, ID1 =/= ID2).
