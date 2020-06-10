-module(operator).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start/1, start_link/1]).
-export([ask/2, shutdown/1]).

%% gen_server
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).

start(_Args) -> gen_server:start(?MODULE, [], []).
start_link(_Args) -> gen_server:start_link(?MODULE, [], []).
shutdown(Pid) -> gen_server:call(Pid, terminate).

ask(Pid, Question) -> gen_server:call(Pid, {ask, Question}).


%% Server functions

init([]) -> {ok, []}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};

handle_call({ask, Question}, _From, State) ->
    Answer = process_question(Question),
    {reply, {ok, Answer}, State}.

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

%% process_question/1 mocks an operator handling a question.
process_question(Question) -> Question.
