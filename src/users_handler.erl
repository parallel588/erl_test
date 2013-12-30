-module(users_handler).
 
%% -behaviour(cowboy_http_handler).

%% Cowboy_http_handler callbacks
-export([
         init/3,
         terminate/3,
         content_types_provided/2,
         allowed_methods/2,
         content_types_accepted/2,
         users_to_json/2,
         all_users/0,
         delete_resource/2,
         delete_completed/2,
         create_or_update_user/2,
         create_user/2,
         update_user/2,
         exists_email/1,
         insert_user/2
        ]).


init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
     {[<<"GET">>, <<"POST">>,<<"PUT">>,<<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, users_to_json}], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {{<<"application">>,<<"x-www-form-urlencoded">>, [{<<"charset">>,<<"utf-8">>}]}, create_or_update_user},
    {{<<"application">>, <<"json">>, []}, create_or_update_user}],
    Req, State}.

create_or_update_user(Req, State) ->
    case cowboy_req:method(Req) of
        {<<"POST">>, Req4} ->
            {_, Req5} = create_user(Req4, State);
        {_, Req4} ->
            {_, Req5} = update_user(Req4, State)
        end,
    {true, Req5, State}.

create_user(Req, _State) ->
    {ok, KeyValues, Req2} = cowboy_req:body_qs(Req),
    io:format("Create ~n"),
    UsersData = jiffy:decode(proplists:get_value(<<"models">>, KeyValues)),
    io:format(standard_error, "Data: ~p~n", [UsersData]),
    [insert_user(UserEmail, [{":email", UserEmail}, {":name", UserName}, {":password", "123456"}]) || { [ {_, _}, {_, UserName}, {_, UserEmail} ] }  <- UsersData],
    {ok, Req2}.

insert_user(Email, Data)->
    io:format(standard_error, "Insert Data: ~p~n", [Data]),
    case exists_email(Email) of
        true ->
            ok;
        false ->
            io:format(standard_error, "Insert Data in table: ~p~n", [Data]),
            sqlite3:sql_exec(erl_test_db, "INSERT INTO users (email, name, password) VALUES (:email, :name, :password)", Data)
    end,
    ok.
    
exists_email(Email) ->
    [_, {_, Rows}] = sqlite3:sql_exec(erl_test_db, "select count(*) from users where email = :email", [{":email", Email}]),
    case Rows of
        [{1}] ->
            Res = true;
        _ ->
            Res = false
    end,
    Res.
    
    
update_user(Req, _State) ->
    {ok, KeyValues, Req2} = cowboy_req:body_qs(Req),
    io:format("Update ~n"),
    UsersData = jiffy:decode(proplists:get_value(<<"models">>, KeyValues)),
    io:format(standard_error, "Data: ~p~n", [UsersData]),
    [
     sqlite3:update(erl_test_db, users, {id, UserId}, [{email, UserEmail}, {name, UserName}]) || {
                     [ {_, UserId}, {_, UserName}, {_, UserEmail} ] }  <- UsersData],
    {ok, Req2}.

users_to_json(Req, State) ->
    JiffyTerm = dict_to_jiffy(all_users()),
    Json = jiffy:encode(JiffyTerm),
    {Json, Req, State}.

delete_resource(Req, State) ->
    {ok, KeyValues, Req2} = cowboy_req:body_qs(Req),
    UsersData = jiffy:decode(proplists:get_value(<<"models">>, KeyValues)),
    io:format(standard_error, "Data: ~p~n", [UsersData]),
    [sqlite3:delete(erl_test_db, users, {id, UserId}) || { [ {_, UserId}, _, _ ] }  <- UsersData],
    {true, Req2, State}.

delete_completed(Req, State) ->
    Req2 = cowboy_req:set_resp_body( <<"{}">>, Req ),
    {true, Req2, State}.

all_users() ->
    [{columns, _}, {rows, AllRows}] = sqlite3:read_all(erl_test_db, users, [id, name, email]),
    [dict:from_list([{id, Id},{name, Name}, {email, Email}]) || {Id, Name, Email} <- AllRows].

dict_to_jiffy(Dict) when is_tuple(Dict) ->
  {[{Key, dict_to_jiffy(Value)} || {Key, Value} <- dict:to_list(Dict)]};

dict_to_jiffy(List) when is_list(List) ->
  [dict_to_jiffy(Item) || Item <- List];

dict_to_jiffy(Value) ->
  Value.

terminate(_Reason, _Req, _State) ->
    ok.
