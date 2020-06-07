-module(sockserv_tests).
-include("erl_playground_pb.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([send/2]).

%% I can't find a better way to mock sockserv:state so I'm copy/pasting its
%% definition for now.
-record(state, {
    socket :: any(), %ranch_transport:socket(),
    transport,
    uid,
    operator,
    username=""
}).

%% send/2 mocks the real TCP send.
send(Socket, Packet) ->
    case Socket of
        discard -> ok;
        _ -> self() ! {incoming, Socket, Packet}
    end.

mock_state(Ref) ->
    #state{socket = Ref,
           transport = ?MODULE}. %% make sockserv use send/2 when sending data

mock_send_request(Req, State) ->
    Packet = utils:add_envelope(Req),
    {noreply, NewState} = sockserv:handle_info({packet, Packet}, State),
    NewState.

create_session_test() ->
    Username = binary:list_to_bin("my username"),
    State = mock_state(discard),
    NewState = mock_send_request(#req{
        type = create_session,
        create_session_data = #create_session {
            username = Username
        }
    }, State),
    ?assertEqual(Username, NewState#state.username).

call_id_test() ->
    CallID = binary:list_to_bin("some rand id"),
    BaseState = mock_state(discard),
    State = BaseState#state{uid=CallID},
    NewState = mock_send_request(#req{
        type = call_id_req
    }, State),
    ?assertEqual(CallID, NewState#state.uid).
