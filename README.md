# Call Center

Call Center is a simulator: an user can *call* the center, an automated
responder lists the available options and the user can dial its choice.

The actual call is done over TCP. The messages exchanged by client and server
are encoded using protobuf.

The services offered by the call center are:

| ID | Name              | Description                                      |
| -- | ----              | -----------                                      |
| 1  | Weather forecasts | Replies telling what's the weather like tomorrow |
| 2  | Jokes of the day  | Replies with a different joke everyday           |
| 3  | Call ID           | Replies with the unique ID of the current call   |
| 3  | Ask an operator   | Starts a conversation to an operator             |

When the call is started from the user, the responder lists all the services
above. The user can dial one of the IDs to make a choice. At the end of the
conversation, the user is *returned* to the initial responder list for starting
again.

## Implementation

### Server

`sockserver` module handles incoming TCP connections spawning a new process for
each of them.

Inside the module are also present the actual handlers based on the protobuf
message type. Handlers build responses gathering data from other services.

Each connection has a `state` attached, containing informations about the
current *call*: username of the user, UID of the call, TCP socket, ....


### Weather service

Weather service (`weather` module) provides a simple API to get forecasts for
this week weather.


### Client

`client` module expose a `client:run()` function that starts the main client
loop. It's meant to be a *frontend* for the underlying `sockclient` library.


### Operators

The *operator* for this exercise is simulated by an echo server.

The echo server replies to a maximum number of messages from the user (default:
3) and for a limited amount of time (default: 10 seconds).

An operator is an Erlang process.


# Run

For executing the app, rebar3 can be used:

```
$ rebar3 shell
```

This will take care of fetching dependencies, compiling, booting services, and
finally starting an Erlang shell.

While in the shell, `client` module can be used:

```
> client:run().
```

---
---

# Original readme below

# erl_playground

An OTP application to start coding without the boring stuff.

## Prerequisites
This project has been written for Mac and Linux environments, theoretically speaking it can run on any environment where a Erlang system is correcty installed, but consider that MS Windows and Erlang are not best buddies. Nowadays it is pretty easy to have Linux systems running in minutes using Virtual Machines, Containers, USB distro or simply double booting your laptop.

In case you use a Mac system, we strongly recommend using [homebrew](https://brew.sh/) to manage all your packages.

**OpenSSL**

Check the correct installation process for you environment.

**Erlang/OTP 21.3**

If you are on Mac, we strongly suggest using [kerl](https://github.com/kerl/kerl) to build and install the proper Erlang version on your system. For other environments you can easily find your installation package on [ErlangSolutions](https://www.erlang-solutions.com/).

## Build & Run

This is a [rebar3](https://www.rebar3.org/) project.

## Compile GPB

Google Protocol Buffer is automatically compiled starting from the included proto file.
[Here](https://developers.google.com/protocol-buffers/) you can find all the information about it.

## What you have out of the box
This is a playgrounf application that allows you to focus on the logic of your system, rather than the boring technical stuff. It includes a basic Erlang/OTP application structure with a TCP client and a TCP server.
