-module(sign_up_handler).

%% Cowboy_http_handler callbacks
-export([
         init/3,
         terminate/3,
         content_types_provided/2,
         allowed_methods/2,
         new_sign_up_to_html/2,
         content_types_accepted/2,
         sign_up_post/2,
         valid_data/3
        ]).

-import(erl_test_app, [
         user_is_signed/1,
         current_user/1,
         redirect_to_sign_in/1,
         redirect_to_root/1]).
-import(users_handler, [
                        insert_user/2,
                        exists_email/1
                       ]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
     {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, new_sign_up_to_html }
     ], Req, State}.

content_types_accepted(Req, State) ->
        {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, sign_up_post}],
                Req, State}.

new_sign_up_to_html(Req, State) ->
    case user_is_signed(Req) of
        false ->
            {ok, Req2} = cowboy_req:reply(200, [], mustache:render(sign_up, "templates/sign_up.mustache"), Req);
        _ ->
            {ok, Req2} =  redirect_to_root(Req)
    end,
    {ok, Req2, State}.

sign_up_post(Req, State) ->
    case user_is_signed(Req) of
        false ->
            { Result, Message, Req2 } = sign_up(Req),
            case Result of
                yes ->
                    {ok, Req3} =  redirect_to_sign_in(Req2);
                _ ->
                    Ctx = dict:from_list([{errormessage, Message}, {errors, true}]),
                    {ok, Req3} = cowboy_req:reply(200, [], mustache:render(sign_up, "templates/sign_up.mustache", Ctx), Req2)
            end;
        _ ->
            {ok, Req3} =  redirect_to_root(Req)
    end,
    {ok, Req3, State}.

sign_up(Req) ->
    {ok, KeyValues, Req2} = cowboy_req:body_qs(Req),
    Name = proplists:get_value(<<"name">>, KeyValues),
    Email = proplists:get_value(<<"email">>, KeyValues),
    Password = proplists:get_value(<<"password">>, KeyValues),
    PasswordConfirmation = proplists:get_value(<<"password_confirmation">>, KeyValues),
    { ValidStatus, ValidResult } = valid_data(Email, Password, PasswordConfirmation),
    
    case ValidStatus of
        true ->
            CheckEmail = exists_email(Email),
            case CheckEmail of
                true ->
                    Res = no,
                    Message = "Email is exists";
                false ->
                    Res = yes,
                    Message = "ok",
                    ok = insert_user(Email, [{":email", Email}, {":name", Name}, {":password", Password}])
            end;
        _ ->
            Res = no,
            Message = ValidResult
    end,
    {Res, Message, Req2}.

valid_data(Email, Password, PasswordConfirmation) ->
    EmailValid = valid_email(Email),
    case EmailValid of 
        ok ->
            PasswordValid = valid_password(Password, PasswordConfirmation),
            case PasswordValid of
                ok -> 
                    Message = '',
                    Res = true;
                _ ->
                    Res = false,
                    Message = PasswordValid
            end;
        _ ->
            Res = false,
            Message = EmailValid
        end,
    { Res, Message }.

valid_password(Password, PasswordConfirmation) ->
    case Password =:= "" of 
        true ->
            Res = "Password is not be blank";
        false ->
            case PasswordConfirmation =:= "" of
                true -> 
                    Res = "Password Confirmation is not be blank";
                false ->
                    case Password =:= PasswordConfirmation of
                        true ->
                            Res = ok;
                        false ->
                            Res = "Password and Confirmation should be equal"
                    end
            end
    end,
    Res.

valid_email(Email) ->
    case Email of
        "" ->
            Res = "Email is not be blank";
        _ ->
            Res = ok
        end,

    Res.

terminate(_Reason, _Req, _State) ->
    ok.

