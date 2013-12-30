-module(sign_in).
-compile(export_all).

start() ->
    Output = mustache:render(sign_in, "sign_in.mustache"),
    io:format(Output, []).
