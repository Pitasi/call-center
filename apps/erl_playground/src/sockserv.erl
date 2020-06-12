-module(sockserv).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("erl_playground_pb.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/4]). -ignore_xref([{start_link, 4}]).
-export([start/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% ------------------------------------------------------------------
%% ranch_protocol Function Exports
%% ------------------------------------------------------------------

-export([init/4]). -ignore_xref([{init, 4}]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {
    socket :: any(), %ranch_transport:socket(),
    transport,
    uid,
    operator,
    username=""
}).
-type state() :: #state{}.

%% user is a record used for chat between users.
-record(user, {
	transport,
	socket
}).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(CB_MODULE, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Definition
%% ------------------------------------------------------------------

start() ->
    {ok, Port} = application:get_env(erl_playground, tcp_port),
    {ok, MaxConnections} = application:get_env(erl_playground, max_connections),

    TcpOptions = [
        {backlog, 100}
    ],

    {ok, _} = ranch:start_listener(
        sockserv_tcp,
        ranch_tcp,
        [{port, Port},
        {num_acceptors, 100}] ++ TcpOptions,
        sockserv,
        [none]
    ),

    ranch:set_max_connections(sockserv_tcp, MaxConnections),
    lager:info("server listening on tcp port ~p", [Port]),
    ok.

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% ------------------------------------------------------------------
%% ranch_protocol Function Definitions
%% ------------------------------------------------------------------

init(Ref, Socket, Transport, [_ProxyProtocol]) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),

    Opts = [{packet, 2}, {packet_size, 16384}, {active, once}, {nodelay, true}],
    _ = Transport:setopts(Socket, Opts),

    State = #state{
        socket = Socket,
        transport = Transport,
        uid = uid:generate()
    },
    gen_server:enter_loop(?MODULE, [], State).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) -> undefined.

handle_cast(Message, State) ->
    _ = lager:notice("unknown handle_cast ~p", [Message]),
    {noreply, State}.

handle_info({tcp, _Port, <<>>}, State) ->
    _ = lager:notice("empty handle_info state: ~p", [State]),
    {noreply, State};
handle_info({tcp, _Port, Packet}, State = #state{socket = Socket}) ->
    self() ! {packet, Packet},
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, _Port}, State) ->
    {stop, normal, State};

handle_info({packet, Packet}, State) ->
    Req = utils:open_envelope(Packet),
    NewState = process_packet(Req, State, utils:unix_timestamp()),
    {noreply, NewState};

handle_info({operator_removed, Ref}, #state{operator = Ref} = State) ->
	send(server_message("[server] Your operator left the chat.~n"), State),
    NewState = State#state{operator = undefined},
	{noreply, NewState};

handle_info(Message, State) ->
    _ = lager:notice("unknown handle_info ~p -- ~p", [Message, State]),
    {noreply, State}.

handle_call(Message, _From, State) ->
    _ = lager:notice("unknown handle_call ~p", [Message]),
    {noreply, State}.

terminate(normal, _State) ->
    _ = lager:info("Goodbye!"),
    ok;
terminate(Reason, _State) ->
    _ = lager:notice("No terminate for ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec process_packet(Req :: #req{}, State :: state(), Now :: integer()) -> NewState :: state().
process_packet(undefined, State, _Now) ->
    _ = lager:notice("client sent invalid packet, ignoring ~p",[State]),
    State;
process_packet(#req{ type = Type } = Req, State = #state{}, _Now) ->
    case handle_request(Type, Req, State) of
        {noreply, NewState} -> NewState;
        {Response, NewState} -> send(Response, NewState), NewState
    end.

send(Response, #state{socket = Socket, transport = Transport}) ->
	send(Response, Socket, Transport).

send(Response, Socket, Transport) ->
    Data = utils:add_envelope(Response),
    Transport:send(Socket, Data).

%% ------------------------------------------------------------------
%% Request handlers
%% ------------------------------------------------------------------

server_message(Msg) ->
    #req{
        type = server_message,
        server_message_data = #server_message {
            message = Msg
        }
    }.

handle_request(create_session, #req{
    create_session_data = #create_session {
        username = UserName
    }
}, State) ->
    NewState = State#state{username=UserName},
    {server_message(io_lib:format("Welcome ~s!~n", [UserName])), NewState};

handle_request(call_id_req, _Req, #state{uid = UID} = State) ->
    {server_message(
        io_lib:format("-----------~n"
                      "| Call ID |~n"
                      "-----------~n"
                      "~s~n", [UID])
      ), State};


handle_request(weather_req, _Req, State) ->
    Timedates = [date:add_days(X) || X <- lists:seq(0, 6)],
    Forecasts = [{Date, weather:for_date(Date)} || {Date, _} <- Timedates],
    {server_message(build_forecasts_message(Forecasts)), State};

handle_request(joke_req, _Req, State) ->
    {server_message(build_joke_message(jokes:of_today())), State};

handle_request(operator_req, _Req, State) ->
	case operator_manager:take() of
        {ok, Ref} ->
            NewState = State#state{operator = Ref},
            {server_message("[server] You are now connected to an operator.~n"), NewState};
        {error, _} ->
            {server_message("[server] No available operators at this time. Try again later~n"), State}
    end;

handle_request(operator_quit_req, _Req, #state{operator = undefined} = State) ->
    {server_message("[server] Bye!~n"), State};

handle_request(operator_quit_req, _Req, #state{operator = Operator} = State)
  when Operator =/= undefined ->
	operator_manager:put(Operator),
    NewState = State#state{operator = undefined},
    {server_message("[server] Bye!~n"), NewState};

handle_request(operator_msg_req, _Req, #state{operator = undefined} = State) ->
    {server_message("[server] You aren't connected to an operator.~n"), State};

handle_request(operator_msg_req, #req{
    operator_msg_data = #operator_message {
        message = Message
    }
}, #state{operator = Operator} = State) ->
    case operator_manager:ask(Operator, Message) of
		{ok, Answer} ->
			{server_message(build_operator_message(Answer)), State};
		{error, _} ->
			NewState = State#state{operator = undefined},
			{server_message("[server] You can't ask more questions.~n"), NewState}
	end;

handle_request(chat_req, _Req, #state{socket = Socket, transport = Transport} = State) ->
	User = #user{socket = Socket, transport = Transport},
	chat_manager:add(User),
	{server_message("[server] You entered a chat room.~n"), State};

handle_request(chat_msg_req, #req{
    chat_msg_data = #chat_message {
        message = Message
    }
}, #state{socket = Socket, transport = Transport, username = Username} = State) ->
	User = #user{socket = Socket, transport = Transport},
	case chat_manager:get_partner(User) of
		{ok, #user{socket = PartnerSocket, transport = PartnerTransport}} ->
			send(server_message(
			   io_lib:format("[~s] ~s~n", [Username, Message])
			), PartnerSocket, PartnerTransport),
			{noreply, State};
		_ ->
			{server_message("[server] You aren't connected to a partner.~n"), State}
	end;

handle_request(chat_quit_req, _Req, #state{socket = Socket, transport = Transport} = State) ->
	User = #user{socket = Socket, transport = Transport},
	case chat_manager:close(User) of
		{ok, undefined} ->
			{server_message("[server] Bye!~n"), State};
		{ok, #user{socket = Partner, transport = Transport}} ->
			send(server_message("[server] Your partner quit the chat.~n"), Partner, Transport),
			{server_message("[server] Bye!~n"), State}
	end.

build_operator_message(Answer) ->
    io_lib:format("[Operator]: ~p~n", [Answer]).

build_forecasts_message(Forecasts) ->
    build_forecasts_message(Forecasts, "---------------------~n"
                                       "| Weather forecasts |~n"
                                       "---------------------~n").

build_forecasts_message([], Msg) -> Msg ++ "~n";
build_forecasts_message([{{Y, M, D}, Weather}|Tl], Msg) ->
    build_forecasts_message(
      Tl,
      io_lib:format("~s- ~B/~B/~B will be ~p~n", [Msg, D, M, Y, Weather])
    ).

build_joke_message(Joke) ->
    io_lib:format("-----------------~n"
                  "| Joke of today |~n"
                  "-----------------~n"
                  "~s~n", [Joke]).
