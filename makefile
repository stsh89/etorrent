dev:
	rebar3 shell

tests:
	rebar3 eunit

shell:
	erl -pa _build/default/lib/etorrent/ebin/

compile:
	rebar3 compile

format:
	rebar3 format
