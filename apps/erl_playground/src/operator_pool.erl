-module(operator_pool).
-behaviour(supervisor).
-export([start/0, init/1, take/1, put/1]).

-define(POOL_NAME, operators).
-define(POOL_SIZE, 3). %% TODO: read this from application config
-define(OPERATOR_MODULE, operator).

%% start/0 starts the operators pool.
start() ->
    supervisor:start_link({local, sandbox}, ?MODULE, []).

%% init/1 is called as supervisor callback.
init([]) ->
    PoolSpecs = [
        poolboy:child_spec(?POOL_NAME, [
            {name, {local, ?POOL_NAME}},
            {worker_module, ?OPERATOR_MODULE},
            {size, ?POOL_SIZE},
            {max_overflow, 0}
        ], [])
    ],
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

%% take/1 takes an idle operator from the pool, waiting Timeout for one to
%% become available.
%% Returns {ok, Ref} or {error, Reason} (i.e. no operators are available and
%% timeout expired).
take(Timeout) ->
    try poolboy:checkout(?POOL_NAME, true, Timeout) of
        Pid -> Pid
    catch
        exit:_ -> {error, no_operators_available}
    end.

%% put/1 puts back an operator in the pool.
put(Operator) ->
    poolboy:checkin(?POOL_NAME, Operator).
