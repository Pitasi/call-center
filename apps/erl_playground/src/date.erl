-module(date).
-export([now/0]).

%% now returns the calendar:datetime tuple of today at current UTC time.
%% E.g. {{2020, 6, 5}, {16, 31, 57}}
now() ->
    calendar:now_to_datetime(erlang:timestamp()).
