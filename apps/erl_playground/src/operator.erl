-module(operator).
-behaviour(gen_server).

-export([start/2, start_link/2]).
-export([ask/2, shutdown/1]).

%% gen_server
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).

start(Timeout, MaxReq) -> gen_server:start(?MODULE, {Timeout, MaxReq}, []).
start_link(Timeout, MaxReq) -> gen_server:start_link(?MODULE, {Timeout, MaxReq}, []).
shutdown(Pid) -> gen_server:call(Pid, terminate).

ask(Pid, Question) -> gen_server:call(Pid, {ask, Question}).


%% Server functions

init({Timeout, MaxReq}) when MaxReq > 0 ->
    init_timeout(Timeout),
    {ok, MaxReq}.

init_timeout(Timeout) ->
    case Timeout of
        infinity -> ok;
        _ -> erlang:send_after(Timeout, self(), timeout)
    end.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};

handle_call({ask, Question}, _From, RemainingQuestions) when RemainingQuestions > 0 ->
    Rem = RemainingQuestions - 1,
    Answer = process_question(Question),
    case Rem of
        0 -> {stop, normal, Answer, Rem};
        _ -> {reply, Answer, Rem}
    end.

handle_cast(Msg, State) ->
    io:format("Unexpected cast: ~p~n",[Msg]),
    {noreply, State}.

handle_info(timeout, State) ->
    {stop, normal, State};

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
