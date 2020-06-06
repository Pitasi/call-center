-module(weather).
-behaviour(gen_server).

-export([start/0, start_link/0]).
-export([for_tomorrow/0, for_date/1, shutdown/0]).

%% gen_server
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).

start() -> gen_server:start({local, ?SERVER}, ?MODULE, [], []).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

shutdown() ->
    gen_server:call(?SERVER, terminate).

for_tomorrow() ->
    {Date, _Time} = date:add_days(1),
    for_date(Date).

for_date(Date) ->
    gen_server:call(?SERVER, {weather, Date}).


%% Server functions

init([]) -> {ok, []}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};

handle_call({weather, Date}, _From, State) ->
    case calendar:valid_date(Date) of
        true -> {reply, get_weather(Date), State};
        false -> {reply, {error, invalid_date}, State}
    end.

handle_cast(Msg, State) ->
    io:format("Unexpected cast: ~p~n",[Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

%% get_weather/1 mocks a function returning a real weather forecast for Date.
get_weather({Y, M, D} = _Date) ->
    Weathers = [
        sunny,
        windy,
        rainy
    ],
    lists:nth(1 + ((Y+M+D) rem length(Weathers)), Weathers).
