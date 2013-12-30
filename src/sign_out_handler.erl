-module(sign_out_handler).
 
-behaviour(cowboy_http_handler).

%% Cowboy_http_handler callbacks
-export([
         init/3,
         handle/2,
         terminate/3
        ]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Req2} = cowboy_session:set('user_id', 0, Req),
    {ok, Req3} = cowboy_req:reply(302, [{<<"Location">>, <<"/">>}], <<>>, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.
