-module(index).
-compile(export_all).

start() ->
    Output = mustache:render(index, "index.mustache"),
    io:format(Output, []).

users() ->
    [{columns, _}, {rows, AllRows}] = sqlite3:read_all(erl_test_db, users, [id, name, email]),
    [dict:from_list([{id, Id},{name, binary_to_list(Name)}, {email, binary_to_list(Email)}]) || {Id, Name, Email} <- AllRows].


