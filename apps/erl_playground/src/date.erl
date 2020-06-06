-module(date).
-export([now/0, add_days/1, add_days/2]).

%% now/0 returns the calendar:datetime tuple of today at current UTC time.
%% E.g. {{2020, 6, 5}, {16, 31, 57}}
now() ->
    calendar:universal_time().

%% add_days/1 is shorthand for add_days/2 using current datetime.
add_days(Days) -> add_days(Days, ?MODULE:now()).

%% add_days/2 adds some days to DateTime, returns a new DateTime.
add_days(Days, DateTime) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    SecondsPerDay = 60*60*24,
    calendar:gregorian_seconds_to_datetime(Seconds + Days * SecondsPerDay).
