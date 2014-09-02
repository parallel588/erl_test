all:
	test -d deps || rebar get-deps
	rebar compile
	@erl -noshell  -pa ebin deps/*/ebin -s erl_test start

run_test:
	rebar eunit skip_deps=true
