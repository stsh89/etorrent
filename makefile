dev:
	rebar3 shell

tests:
	rebar3 eunit

shell:
	erl -pa _build/default/lib/etorrent/ebin/ -pa _build/default/lib/benc/ebin/
compile:
	rebar3 compile
