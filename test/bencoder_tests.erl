-module(bencoder_tests).
-include_lib("eunit/include/eunit.hrl").

decode_integer_test() ->
  ?assertMatch({int, 0}, bencoder:decode(<<"i0e">>)),
  ?assertMatch({int, 1}, bencoder:decode(<<"i1e">>)),
  ?assertMatch({int, -1}, bencoder:decode(<<"i-1e">>)),
  ?assertMatch({int, 32}, bencoder:decode(<<"i32e">>)),
  ?assertMatch({int, 928374}, bencoder:decode(<<"i928374e">>)),
  ?assertMatch({error, badarg}, bencoder:decode(<<"">>)),
  ?assertMatch({error, badarg}, bencoder:decode(<<"x">>)),
  ?assertMatch({error, badarg}, bencoder:decode(<<"e">>)),
  ?assertMatch({error, badarg}, bencoder:decode(<<"i0">>)),
  ?assertMatch({error, badarg}, bencoder:decode(<<"i0ee">>)).

decode_string_test() ->
  ?assertMatch({str, <<>>}, bencoder:decode(<<"0:">>)),
  ?assertMatch({str, <<"test">>}, bencoder:decode(<<"4:test">>)),
  ?assertMatch({str, <<"longstring">>}, bencoder:decode(<<"10:longstring">>)),
  ?assertMatch({error, badarg}, bencoder:decode(<<"6:test">>)),
  ?assertMatch({error, badarg}, bencoder:decode(<<"3:test">>)).

decode_list_test() ->
  ?assertMatch({list, []}, bencoder:decode(<<"le">>)),
  ?assertMatch({list, [{int, 0}]}, bencoder:decode(<<"li0ee">>)),
  ?assertMatch({list, [{int, 0}, {int, 0}]}, bencoder:decode(<<"li0ei0ee">>)),
  ?assertMatch({list, [{int, 0}, {int, 0}, {int, 0}]}, bencoder:decode(<<"li0ei0ei0ee">>)),
  ?assertMatch({error, badarg}, bencoder:decode(<<"lee">>)).

decode_dict_test() ->
  ?assertMatch({dict, #{}}, bencoder:decode(<<"de">>)),

  {Kind, Map} = bencoder:decode(<<"d4:namei0ee">>),
  ?assertMatch(dict, Kind),
  ?assertMatch({int, 0}, maps:get({str, <<"name">>}, Map)),

  {Kind1, Map1} = bencoder:decode(<<"d4:name4:Stane">>),
  ?assertMatch(dict, Kind1),
  ?assertMatch({str, <<"Stan">>}, maps:get({str, <<"name">>}, Map1)).
