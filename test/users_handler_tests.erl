-module(users_handler_tests).
-compile(export_all).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


users_handler_test_() ->
  TestFunctions = [F || {F,0} <- ?MODULE:module_info(exports),
                            lists:prefix("test_", atom_to_list(F))],
    sqlite3:open(erl_test_db),
     case lists:member(users, sqlite3:list_tables(erl_test_db)) of
         false ->
             TableUserInfo = [{id, integer, [{primary_key, [asc, autoincrement]}]},
                              {name, text},
                              {email, text, [not_null, unique]}, 
                              {password, text, not_null}],
             ok = sqlite3:create_table(erl_test_db, users, TableUserInfo),
             ok;
        true -> ok
    end,
  {foreach,
    fun setup/0,
    fun teardown/1,
    [{atom_to_list(F), fun ?MODULE:F/0} || F <- TestFunctions]
  }.


setup() ->
    ok.

teardown(_) ->
    ok.


test_insert_user() ->
    sqlite3:sql_exec(erl_test_db, "DELETE FROM users;"),
    Data =  [{":email", "test@test.com"}, {":name", "testname"}, {":password", "123456"}],
    users_handler:insert_user("test@test.com", Data),
    ?assertEqual(true, users_handler:exists_email("test@test.com")).

test_exists_user() ->
    sqlite3:sql_exec(erl_test_db, "DELETE FROM users;"),
    Data =  [{":email", "test@test.com"}, {":name", "testname"}, {":password", "123456"}],
    sqlite3:sql_exec(erl_test_db, "INSERT INTO users (email, name, password) VALUES (:email, :name, :password)", Data),
    ?assertEqual(true, users_handler:exists_email("test@test.com")),
    ?assertEqual(false, users_handler:exists_email("test12@test.com")).


test_users_to_json() ->
    sqlite3:sql_exec(erl_test_db, "DELETE FROM users"),
    Data =  [{":email", "test11@test.com"}, {":name", "testname"}, {":password", "123456"}],
    sqlite3:sql_exec(erl_test_db, "INSERT INTO users (email, name, password) VALUES (:email, :name, :password)", Data),
    [{columns,_},{rows,[{UserId,_,_,_}]}] = sqlite3:read(erl_test_db, users, {email, "test11@test.com"}),
    UsersJson = users_handler:users_to_json(req, state),
    ?assertEqual({<<"[{\"id\":",(integer_to_binary(UserId))/binary,",\"name\":\"testname\",\"email\":\"test11@test.com\"}]">>, req, state}, UsersJson ).


test_all_users() ->
    sqlite3:sql_exec(erl_test_db, "DELETE FROM users"),
    Data =  [{":email", "test11@test.com"}, {":name", "testname"}, {":password", "123456"}],
    Data1 =  [{":email", "test22@test.com"}, {":name", "testname"}, {":password", "123456"}],
    sqlite3:sql_exec(erl_test_db, "INSERT INTO users (email, name, password) VALUES (:email, :name, :password)", Data),
    sqlite3:sql_exec(erl_test_db, "INSERT INTO users (email, name, password) VALUES (:email, :name, :password)", Data1),
    [{columns, _}, {rows, AllRows}] = sqlite3:read_all(erl_test_db, users, [id, name, email]),
    UsersData = [dict:from_list([{id, Id},{name, Name}, {email, Email}]) || {Id, Name, Email} <- AllRows],
    ?assertEqual(UsersData, users_handler:all_users()).

-endif.
