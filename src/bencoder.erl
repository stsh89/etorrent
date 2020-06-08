-module(bencoder).
-export([decode/1]).

-spec decode(Data::binary()) -> {ok, integer()} | {error, badarg}.
decode(Data) ->
  do_decode(Data).

do_decode(<<"i", T/binary>>) ->
  case do_integer_decode([], T) of
    {ok, Value} -> {ok, Value};
    {error, badarg} -> {error, badarg}
  end;

do_decode(_) ->
  {error, badarg}.

do_integer_decode(_Acc, <<>>) ->
  {error, badarg};

do_integer_decode(Acc, <<"e", _T/binary>>) ->
  {ok, list_to_integer(Acc)};

do_integer_decode(Acc, <<H, T/binary>>) ->
  do_integer_decode(Acc ++ [H], T).
