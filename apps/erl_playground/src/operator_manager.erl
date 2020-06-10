-module(operator_manager).

-behaviour(gen_server).

-export([start/0, start_link/0]).
-export([take/0, put/1, ask/2, shutdown/0]).

%% gen_server
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).
-define(POOL_TIMEOUT, application:get_env(erl_playground, operator_pool_timeout, 5000)).
-define(OP_MAXREQ, application:get_env(erl_playground, operator_max_requests, 3)).
-define(OP_MAXTIME, application:get_env(erl_playground, operator_timeout, 10000)).
-record(instance, {from, operator, questions}).

start() -> gen_server:start({local, ?SERVER}, ?MODULE, [], []).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

shutdown() -> gen_server:call(?SERVER, terminate).

take() -> gen_server:call(?SERVER, {take, self()}).
put(Ref) -> gen_server:cast(?SERVER, {put, Ref}).
ask(Ref, Question) -> gen_server:call(?SERVER, {ask, Ref, Question}).


%% Server functions

init([]) -> {ok, maps:new()}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};

handle_call({take, CallbackPid}, _From, State) ->
	case operator_pool:take(?POOL_TIMEOUT) of
		{ok, Pid} ->
			Ref = make_ref(),
			Inst = #instance{from=CallbackPid, operator=Pid, questions=?OP_MAXREQ},
			NewState = maps:put(Ref, Inst, State),
			erlang:send_after(?OP_MAXTIME, self(), {timeout, Ref}),
			{reply, {ok, Ref}, NewState};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;

handle_call({ask, Ref, Question}, _From, State) ->
	case maps:find(Ref, State) of
		error ->
			{reply, {error, invalid_ref}, State};
		{ok, #instance{questions=0}} ->
			%% this case is supposed to be called only if ?OP_MAXREQ is set to 0
			NewState = remove(Ref, State),
			{reply, {error, no_questions_left}, NewState};
		{ok, #instance{operator=Pid, questions=Questions} = I} ->
			Answer = operator:ask(Pid, Question),
			NewState = case Questions of
				1 -> remove(Ref, State);
				_ -> maps:update(Ref, I#instance{questions=Questions-1}, State)
			end,
			{reply, Answer, NewState}
	end.

handle_cast({put, Ref}, State) ->
	NewState = remove(Ref, State),
	{noreply, NewState};

handle_cast(Msg, State) ->
    io:format("Unexpected cast: ~p~n",[Msg]),
    {noreply, State}.

handle_info({timeout, Ref}, State) ->
	remove(Ref, State),
	{noreply, State};

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

remove(Ref, State) ->
	case maps:find(Ref, State) of
		error ->
			State;
		{ok, #instance{operator=Pid, from=From}} ->
			From ! {operator_removed, Ref},
			operator_pool:put(Pid),
			maps:remove(Ref, State)
	end.
