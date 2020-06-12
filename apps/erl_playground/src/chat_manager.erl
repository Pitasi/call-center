-module(chat_manager).
-behaviour(gen_server).

-export([start/0, start_link/0]).
-export([add/1, get_partner/1, close/1, shutdown/0]).

%% gen_server
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).
-record(state, {waiting, matched=maps:new()}).

start() -> gen_server:start({local, ?SERVER}, ?MODULE, [], []).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
shutdown() -> gen_server:call(?SERVER, terminate).

add(User) -> gen_server:call(?SERVER, {add, User}).
get_partner(User) -> gen_server:call(?SERVER, {get_partner, User}).
close(User) -> gen_server:call(?SERVER, {close, User}).


%% Server functions

init([]) -> {ok, #state{}}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};

handle_call({add, User}, _From, #state{waiting = undefined} = State) ->
	NewState = State#state{waiting = User},
	{reply, ok, NewState};

handle_call({add, User}, _From, #state{waiting = WaitingUser, matched = Matched} = State) ->
	NewMatched = maps:put(User, WaitingUser, Matched),
	NewMatched2 = maps:put(WaitingUser, User, NewMatched),
	NewState = State#state{waiting = undefined, matched = NewMatched2},
	{reply, ok, NewState};

handle_call({get_partner, User}, _From, #state{matched = Matched} = State) ->
	{reply, maps:find(User, Matched), State};

handle_call({close, User}, _From, #state{matched = Matched} = State) ->
	case maps:find(User, Matched) of
		{ok, Partner} ->
			NewMatched = maps:remove(Partner, Matched),
			NewMatched2 = maps:remove(User, NewMatched),
			NewState = State#state{matched = NewMatched2},
			{reply, {ok, Partner}, NewState};
		_ ->
			NewMatched = maps:remove(User, Matched),
			NewState = State#state{matched = NewMatched},
			{reply, {ok, undefined}, NewState}
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
