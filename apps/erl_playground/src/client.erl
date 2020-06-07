-module(client).

-export([run/0]).

-record(service, {name, handler}).

services() ->
    [#service{name = "Weather forecasts", handler=fun handle_weather/0},
     #service{name = "Joke of the day", handler=fun handle_joke/0},
     #service{name = "My caller ID", handler=fun handle_caller_id/0},
     #service{name = "Ask an operator", handler=fun handle_ask_operator/0},
     #service{name = "Quit", handler=fun () -> quit end}
    ].

run() ->
    init(),
    loop(services()).

init() ->
    sockclient:connect(self()),
    banner(),
    pick_username().

loop(Services) ->
    case main_menu(Services) of
        quit -> ok;
        _ -> loop(Services)
    end.

banner() ->
    io:format("[ Call Center v1.0 ]~n").

pick_username() ->
    Username = ask("Please insert your username: "),
    sockclient:send_create_session(Username),
    flush().

main_menu(Services) ->
    io:format("~n"
              "╔═══════════╗~n"
              "║ MAIN MENU ║~n"
              "╚═══════════╝~n"),
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
    sockclient:send_weather_req(),
    flush().

handle_joke() ->
    sockclient:send_joke_req(),
    flush().

handle_caller_id() ->
    sockclient:send_call_id_req(),
    flush().

handle_ask_operator() ->
    sockclient:send_operator_req(),
    flush(),
    io:format("Write 'bye' to quit chat.~n"),
    operator_chat_loop().

operator_chat_loop() ->
    case ask("> ") of
        "bye" ->
            sockclient:send_operator_quit_req(),
            flush(),
            ok;
        Msg ->
            sockclient:send_operator_msg_req(Msg),
            flush(),
            operator_chat_loop()
    end.

ask(Prompt) ->
    case io:get_line(Prompt) of
        eof ->
            io:format("Invalid input."),
            ask(Prompt);
        {error, Desc} ->
            io:format("Error: ~s", [Desc]),
            ask(Prompt);
        Input ->
            case string:trim(Input) of
                "" -> ask(Prompt);
                S -> S
            end
    end.

flush() ->
    receive
        Message ->
            io:format(Message),
            flush()
    after 100 ->
        ok
    end.
