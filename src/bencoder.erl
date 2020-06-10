-module(bencoder).
-export([decode/1]).

-type benint() :: {int, integer()}.
-type benstr() :: {str, binary()}.
-type benlist() :: {list, list(benitem())}.
-type benitem() :: benint() | benstr() | benlist().

-spec decode(Data::binary()) -> benitem() | {error, badarg}.
decode(Data) ->
  case do_decode(Data) of
    {BenitemCode, Value, <<>>} -> {BenitemCode, Value};
    {_BenitemCode, _Value, _T} -> {error, badarg};
    {error, badarg} -> {error, badarg}
  end.

do_decode(<<"i", T/binary>>) ->
  case do_integer_decode([], T) of
    {ok, Value, Data} -> {int, Value, Data};
    {error, badarg} -> {error, badarg}
  end;

do_decode(<<H, T/binary>>) when H >= $0, H =< $9 ->
  case do_string_decode([H], T) of
    {ok, Value, Data} -> {str, Value, Data};
    {error, badarg} -> {error, badarg}
  end;

do_decode(<<"l", T/binary>>) ->
  case do_list_decode([], T) of
    {ok, Value, Data} -> {list, Value, Data};
    {error, badarg} -> {error, badarg}
  end;

do_decode(_) ->
  {error, badarg}.

do_list_decode(Acc, <<"e", T/binary>>) ->
  {ok, Acc, T};

do_list_decode(Acc, Data) ->
  case do_decode(Data) of
    {BenitemCode, Value, T} -> do_list_decode(Acc ++ [{BenitemCode, Value}], T);
    {error, badarg} -> {error, badarg}
  end.

do_string_decode(Acc, <<":", T/binary>>) ->
  do_string_reduce([], list_to_integer(Acc), T);

do_string_decode(Acc, <<H, T/binary>>) when H >= $0, H =< $9 ->
  do_string_decode(Acc ++ [H], T);

do_string_decode(_Acc, _Data) ->
  {error, badarg}.

do_string_reduce(_Acc, Times, <<>>) when Times > 0 ->
  {error, badarg};

do_string_reduce(Acc, 0, Data) ->
  {ok, list_to_binary(Acc), Data};

do_string_reduce(Acc, Times, <<H, T/binary>>) ->
  do_string_reduce(Acc ++ [H], Times - 1, T).

do_integer_decode(Acc, <<"e", T/binary>>) ->
  {ok, list_to_integer(Acc), T};

do_integer_decode(Acc, <<H, T/binary>>) ->
  do_integer_decode(Acc ++ [H], T);

do_integer_decode(_Acc, <<>>) ->
  {error, badarg};

do_integer_decode(_Acc, _Data) ->
  {error, badarg}.
