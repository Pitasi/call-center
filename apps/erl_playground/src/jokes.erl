-module(jokes).
-behaviour(gen_server).

-export([start/0, start_link/0]).
-export([of_today/0, shutdown/0]).

%% gen_server
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).

start() -> gen_server:start({local, ?SERVER}, ?MODULE, [], []).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

shutdown() ->
    gen_server:call(?SERVER, terminate).

of_today() ->
    gen_server:call(?SERVER, {joke}).


%% Server functions

init([]) -> {ok, []}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};

handle_call({joke}, _From, State) ->
    {reply, joke(date:now()), State}.

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

%% joke/1 returns a different joke (string) for different days.
joke({{Y, M, D}, _}) ->
    Jokes = [
        "joke1",
        "joke2",
        "joke3",
        "joke4"
    ],
    lists:nth(1 + ((Y+M+D) rem length(Jokes)), Jokes).
