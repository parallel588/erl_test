-module(erl_test_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, 
         user_is_signed/1,
         current_user/1,
         redirect_to_sign_in/1,
         redirect_to_root/1
        ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    _ = init_db(),
    Routes    = routes(),
    Dispatch  = cowboy_router:compile(Routes),
    Port      = port(),
    TransOpts = [{port, Port}],
    ProtoOpts = [
                 {env, [{dispatch, Dispatch}]}, 
                 {onrequest, fun cowboy_session:on_request/1}
                ],
    {ok, _}   = cowboy:start_http(http, 100, TransOpts, ProtoOpts),
    erl_test_sup:start_link().

stop(_State) ->
    close_db(),
    ok.

user_is_signed(Req) ->
    { Value, _ } = cowboy_session:get('user_id', Req),
    
    case Value of
        undefined ->
            Res = false;
        _ ->
            [_, {_, Rows}] = sqlite3:sql_exec(erl_test_db, "select count(*) from users where id = :id", [{":id", Value}]),
            case Rows of
                [{1}] ->
                    Res = true;
                _ ->
                    Res = false
            end
    end,
    Res.

current_user(Req) ->
    { Value, _ } = cowboy_session:get('user_id', Req),
    [_, {_, [Row]}] = sqlite3:sql_exec(erl_test_db, "select * from users where id = :id LIMIT 1", [{":id", Value}]),
    {UserId, UserName, UserEmail, _} = Row,
    dict:from_list([{id, UserId},{name, UserName}, {email, UserEmail}]).

close_db() ->
    sqlite3:close(erl_test_db).

init_db() ->
    erlang:display('init db ---------------'),
    sqlite3:open(erl_test_db),
    case lists:member(users, sqlite3:list_tables(erl_test_db)) of
        false ->
            TableUserInfo = [{id, integer, [{primary_key, [asc, autoincrement]}]},
                             {name, text},
                             {email, text, [not_null, unique]}, 
                             {password, text, not_null}],
            ok = sqlite3:create_table(erl_test_db, users, TableUserInfo),
            ok = seed_db(),
            ok;
        true -> ok
    end,
    ok.

seed_db() ->
    erlang:display('seed db ---------------'),
    Records = [ [{name, "Test Name #" ++ integer_to_list(I)},
                 {email, "test" ++ integer_to_list(I) ++ "@example.com"}, 
                 {password, "123456"}
                ] ||  I <- lists:seq(1, 3000) ],
    _ = sqlite3:write_many(erl_test_db, users, Records),
    ok.

redirect_to_sign_in(Req) ->
    {ok, Req2} = cowboy_req:reply(302, [{<<"Location">>, <<"/sign_in">>}], <<>>, Req),
    {ok, Req2}.

redirect_to_root(Req) ->
    {ok, Req2} = cowboy_req:reply(302, [{<<"Location">>, <<"/">>}], <<>>, Req),
    {ok, Req2}.

routes() ->
    [
     {'_', [
            {"/js/[...]", cowboy_static, { dir, "priv/js"}},
            {"/styles/[...]", cowboy_static, { dir, "priv/styles"}},
            {"/img/[...]", cowboy_static,  {dir, "priv/img"}},
            {"/fonts/[...]", cowboy_static, { dir, "priv/fonts"}},
            {"/", erl_test_handler, []},
            {"/sign_in", sign_in_handler, []},
            {"/sign_up", sign_up_handler, []},
            {"/sign_out", sign_out_handler, []},
            {"/users", users_handler, []}
           ]}
    ].

port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.
