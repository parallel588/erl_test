-module(erl_test).

-export([start/0, stop/0]).


start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = cowboy_session:start(),
    ok = application:start(erl_test).

     
stop() ->
    ok = application:stop(crypto),
    ok = application:stop(ranch),
    ok = application:stop(cowboy),
    ok = cowboy_session:stop(),
    ok = application:stop(erl_test).    
