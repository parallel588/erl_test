-module(erl_test_handler).

-behaviour(cowboy_http_handler).

%% Cowboy_http_handler callbacks
-export([
         init/3,
         handle/2,
         terminate/3
        ]).
-import(erl_test_app, [
         user_is_signed/1,
         current_user/1,
         redirect_to_sign_in/1,
         redirect_to_root/1]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    case user_is_signed(Req) of
        false ->
            {ok, Req2} = redirect_to_sign_in(Req);
        _ ->
            {_, CurrentUserName} = dict:find(name, current_user(Req)),
            Ctx = dict:from_list([ {current_user, binary_to_list(CurrentUserName)} ]),
            {ok, Req2} = cowboy_req:reply(200, [], mustache:render(index, "templates/index.mustache",Ctx), Req)
    end,
    {ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
    ok.
