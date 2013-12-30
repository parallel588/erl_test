-module(sign_up).
-compile(export_all).

start() ->
    Output = mustache:render(sign_up, "sign_up.mustache"),
    io:format(Output, []).
