-module(uid).
-export([generate/0]).

%% generate/0 returns a random string.
%% Do not use this in production since the string is very short and generated
%% naively.
generate() ->
    B = crypto:strong_rand_bytes(8),
    base64:encode(B).
