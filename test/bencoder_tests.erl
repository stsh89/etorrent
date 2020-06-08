-module(bencoder_tests).
-include_lib("eunit/include/eunit.hrl").

decode_integer_test() ->
  ?assertMatch({ok, 0}, bencoder:decode(list_to_binary("i0e"))),
  ?assertMatch({ok, 1}, bencoder:decode(list_to_binary("i1e"))),
  ?assertMatch({ok, -1}, bencoder:decode(list_to_binary("i-1e"))),
  ?assertMatch({ok, 32}, bencoder:decode(list_to_binary("i32e"))),
  ?assertMatch({ok, 928374}, bencoder:decode(list_to_binary("i928374e"))),
  ?assertMatch({error, badarg}, bencoder:decode(list_to_binary(""))),
  ?assertMatch({error, badarg}, bencoder:decode(list_to_binary("x"))),
  ?assertMatch({error, badarg}, bencoder:decode(list_to_binary("e"))),
  ?assertMatch({error, badarg}, bencoder:decode(list_to_binary("i0"))).
