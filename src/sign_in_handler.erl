-module(sign_in_handler).

%% -behaviour(cowboy_http_handler).

%% Cowboy_http_handler callbacks
-export([
         init/3,
         terminate/3,
         content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2,
         new_session_html/2,
         create_session_html/2
        ]).

-import(erl_test_app, [
         user_is_signed/1,
         current_user/1,
         redirect_to_sign_in/1,
         redirect_to_root/1]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, new_session_html }
     ], Req, State}.

content_types_accepted(Req, State) ->
        {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_session_html}],
                Req, State}.

new_session_html(Req, State) ->
    case user_is_signed(Req) of
        false ->
            {ok, Req2} = cowboy_req:reply(200, [], mustache:render(sign_in, "templates/sign_in.mustache"), Req);

        _ ->
            {ok, Req2} =  redirect_to_root(Req)
    end,
    {ok, Req2, State}.

create_session_html(Req, State) ->
    case user_is_signed(Req) of
        false ->
            {_, Result, Message, _, _ } = sign_in(Req, State),
            case Result of
                no ->
                    Ctx = dict:from_list([{errormessage, Message}, {errors, true}]),
                    {ok, Req2} = cowboy_req:reply(200, [], mustache:render(sign_in, "templates/sign_in.mustache", Ctx), Req);
                yes ->
                    {ok, Req2} = cowboy_req:reply(302, [{<<"Location">>, <<"/">>}], <<>>, Req)
            end;
        _ ->
            {ok, Req2} =  redirect_to_root(Req)
    end,
    {ok, Req2, State}.

sign_in(Req, State) ->
    {ok, KeyValues, Req2} = cowboy_req:body_qs(Req),
    Email = proplists:get_value(<<"email">>, KeyValues),
    Password = proplists:get_value(<<"password">>, KeyValues),
    [_, {_, Row}]  = sqlite3:sql_exec(erl_test_db, "select * from users where email = :email and password = :password LIMIT 1", [{":email", Email}, {":password", Password}]),
    case Row of
        [] ->
            Message = "Creditintails is invalid",
            Res = no,
            Req3 = Req2;
        _ ->
            [{UserId, _, _, _}] = Row,
            Res = yes,
            Message = "",
            {ok, Req3} = cowboy_session:set('user_id', UserId, Req2)
    end,
    { ok, Res, Message, Req3, State }.

terminate(_Reason, _Req, _State) ->
    ok.
