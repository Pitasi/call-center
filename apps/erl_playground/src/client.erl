-module(client).

-export([run/0]).

-record(service, {name, handler}).

services() ->
    [#service{name = "Weather forecasts", handler=fun handle_weather/0},
     #service{name = "Joke of the day", handler=fun handle_joke/0},
     #service{name = "My caller ID", handler=fun handle_caller_id/0},
     #service{name = "Ask an operator", handler=fun handle_ask_operator/0}
    ].

run() ->
    init(),
    loop(services()).

init() ->
    sockclient:connect(),
    banner().

loop(Services) ->
    help(Services),
    loop(Services).

banner() ->
    io:format("[ Call Center v1.0 ]~n").

help(Services) ->
    io:format(build_help_message(Services)),
    X = help_ask(length(Services)),
    S = lists:nth(X, Services),
    (S#service.handler)().

build_help_message(Services) -> build_help_message("", 1, Services).
build_help_message(Msg, _, []) -> Msg;
build_help_message(Msg, N, [#service{name=Name}|Services]) ->
    NewMsg = io_lib:format("~s~b. ~p~n", [Msg, N, Name]),
    build_help_message(NewMsg, N+1, Services).

help_ask(NOptions) ->
    case io:fread("Choose an option> ", "~d") of
        {ok, [X]} when X >= 1 andalso X =< NOptions -> X;
        _ -> io:format("Invalid answer.~n"), help_ask(NOptions)
    end.

handle_weather() ->
    sockclient:send_weather_req().

handle_joke() ->
    sockclient:send_joke_req().

handle_caller_id() ->
    sockclient:send_call_id_req().

handle_ask_operator() ->
    sockclient:send_operator_req(),
    io:format("Write 'bye' to quit chat."),
    operator_chat_loop().

operator_chat_loop() ->
    case io:fread("> ", "~s") of
        {ok, ["bye"]} ->
            sockclient:send_operator_quit_req(),
            ok;
        {ok, [Msg]} ->
            sockclient:send_operator_msg_req(Msg),
            operator_chat_loop();
        _ ->
            io:format("Invalid message.~n"),
            operator_chat_loop()
    end.
