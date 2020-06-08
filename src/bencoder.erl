-module(bencoder).
-export([decode/1]).

-type benint() :: {int, integer()}.
-type benstr() :: {str, binary()}.
-type benlist() :: {list, list(benint() | benstr())}.

-spec decode(Data::binary()) -> benint() | benstr() | benlist() | {error, badarg}.
decode(Data) ->
  do_decode(Data).

do_decode(<<"i", T/binary>>) ->
  case do_integer_decode([], T) of
    {ok, Value} -> {int, Value};
    {error, badarg} -> {error, badarg}
  end;

do_decode(<<H, T/binary>>) when H >= $0, H =< $9 ->
  case do_string_decode([H], T) of
    {ok, Value} -> {str, Value};
    {error, badarg} -> {error, badarg}
  end;

do_decode(_) ->
  {error, badarg}.

do_string_decode(Acc, <<":", T/binary>>) ->
  try
    {ok, binary:part(T, {0, list_to_integer(Acc)})}
  catch
    _:badarg ->
      {error, badarg}
  end;

do_string_decode(Acc, <<H, T/binary>>) when H >= $0, H =< $9 ->
  do_string_decode(Acc ++ [H], T);

do_string_decode(_Acc, _Data) ->
  {error, badarg}.

do_integer_decode(Acc, <<"e", _T/binary>>) ->
  {ok, list_to_integer(Acc)};

do_integer_decode(Acc, <<H, T/binary>>) ->
  do_integer_decode(Acc ++ [H], T);

do_integer_decode(_Acc, <<>>) ->
  {error, badarg};

do_integer_decode(_Acc, _Data) ->
  {error, badarg}.
