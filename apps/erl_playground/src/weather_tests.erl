-module(weather_tests).

-include_lib("eunit/include/eunit.hrl").

weather_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun (_) ->
         [for_date_weather_is_constant(),
          for_tomorrow_valid_result(),
          for_future_dates_valid_result()]
     end}.

start() -> weather:start_link().

stop(_) -> weather:shutdown().

for_date_weather_is_constant() ->
    Date = {2020, 01, 01},
    Weather = weather:for_date(Date),
    Weather2 = weather:for_date(Date),
    ?_assertEqual(Weather, Weather2).

for_tomorrow_valid_result() ->
    W = weather:for_tomorrow(),
    ?_assertMatch(true, valid_weather(W)).

for_future_dates_valid_result() ->
    [?_assertMatch(true, valid_weather(weather:for_date(Date))) ||
     Date <- future_dates([], 100)].

future_dates(Acc, 0) -> Acc;
future_dates(Acc, Rem) ->
    {Date, _} = date:add_days(Rem),
    future_dates([Date | Acc], Rem - 1).

valid_weather(W) ->
    W =:= sunny orelse W =:= windy orelse W =:= rainy.
