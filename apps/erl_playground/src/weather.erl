-module(weather).
-behaviour(gen_server).

-export([start/0, start_link/0]).
-export([tomorrow/0, shutdown/0]).

%% gen_server
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).

start() -> gen_server:start({local, ?SERVER}, ?MODULE, [], []).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

tomorrow() ->
    gen_server:call(?SERVER, {weather, date:tomorrow()}).

shutdown() ->
    gen_server:call(?SERVER, terminate).


%% Server functions

init([]) -> {ok, []}. %% no treatment of info here!

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};

handle_call({weather, Date}, _From, State) ->
    % calendar:valid_date(Date)
    {reply, "TODO", State}.

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
