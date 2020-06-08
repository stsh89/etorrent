-module(bencoder_tests).
-include_lib("eunit/include/eunit.hrl").

decode_integer_test() ->
  ?assertMatch({integer, 0}, bencoder:decode(<<"i0e">>)),
  ?assertMatch({integer, 1}, bencoder:decode(<<"i1e">>)),
  ?assertMatch({integer, -1}, bencoder:decode(<<"i-1e">>)),
  ?assertMatch({integer, 32}, bencoder:decode(<<"i32e">>)),
  ?assertMatch({integer, 928374}, bencoder:decode(<<"i928374e">>)),
  ?assertMatch({error, badarg}, bencoder:decode(<<"">>)),
  ?assertMatch({error, badarg}, bencoder:decode(<<"x">>)),
  ?assertMatch({error, badarg}, bencoder:decode(<<"e">>)),
  ?assertMatch({error, badarg}, bencoder:decode(<<"i0">>)).

decode_string_test() ->
  ?assertMatch({string, <<"">>}, bencoder:decode(<<"0:">>)),
  ?assertMatch({string, <<"test">>}, bencoder:decode(<<"4:test">>)),
  ?assertMatch({string, <<"longstring">>}, bencoder:decode(<<"10:longstring">>)),
  ?assertMatch({string, <<"broken">>}, bencoder:decode(<<"6:brokenstring">>)),
  ?assertMatch({error, badarg}, bencoder:decode(<<"6:test">>)).
