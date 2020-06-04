-module(erl_playground_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Restart), {I, {I, start_link, []}, Restart, 5000, Type, [I]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->

    MaxR = 1000, % how many times
    MaxT = 10, % in how many seconds

    {ok, { {one_for_all, MaxR, MaxT}, [
    	?CHILD(ranch_sup, supervisor),
        ?CHILD(node_boot, worker, transient)
    ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
