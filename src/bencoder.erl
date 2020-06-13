-module(bencoder).

-export([decode/1]).

-type benint() :: {int, integer()}.
-type benstr() :: {str, binary()}.
-type benlist() :: {list, list()}.
-type bendict() :: {dict, #{}}.
-type benitem() :: benint() | benstr() | benlist() | bendict().

-spec decode(Data :: binary()) -> benitem() | {error, badarg}.
decode(<<Data/binary>>) ->
  case do_decode(Data) of
    {BenitemCode, Value, <<>>} ->
      {BenitemCode, Value};
    {_BenitemCode, _Value, _T} ->
      {error, badarg};
    {error, badarg} ->
      {error, badarg}
  end.

do_decode(<<"i", T/binary>>) ->
  case decode_integer([], T) of
    {ok, Value, Data} ->
      {int, Value, Data};
    {error, badarg} ->
      {error, badarg}
  end;
do_decode(<<H, T/binary>>) when H >= $0, H =< $9 ->
  case decode_string([H], T) of
    {ok, Value, Data} ->
      {str, Value, Data};
    {error, badarg} ->
      {error, badarg}
  end;
do_decode(<<"l", T/binary>>) ->
  case decode_list([], T) of
    {ok, Value, Data} ->
      {list, Value, Data};
    {error, badarg} ->
      {error, badarg}
  end;
do_decode(<<"d", T/binary>>) ->
  case decode_dict(#{}, T) of
    {ok, Value, Data} ->
      {dict, Value, Data};
    {error, badarg} ->
      {error, badarg}
  end;
do_decode(<<_Data/binary>>) ->
  {error, badarg}.

decode_dict(Acc, <<"e", T/binary>>) ->
  {ok, Acc, T};
decode_dict(Acc, <<H, T/binary>>) ->
  case decode_string([H], T) of
    {ok, Key, Data} ->
      case do_decode(Data) of
        {BenitemCode, Value, L} ->
          decode_dict(maps:put({str, Key}, {BenitemCode, Value}, Acc), L);
        {error, badarg} ->
          {error, badarg}
      end;
    {error, badarg} ->
      {error, badarg}
  end.

decode_list(Acc, <<"e", T/binary>>) ->
  {ok, lists:reverse(Acc), T};
decode_list(Acc, <<Data/binary>>) ->
  case do_decode(Data) of
    {BenitemCode, Value, T} ->
      decode_list([{BenitemCode, Value} | Acc], T);
    {error, badarg} ->
      {error, badarg}
  end.

decode_string(Acc, <<":", T/binary>>) ->
  reduce_string([], list_to_integer(lists:reverse(Acc)), T);
decode_string(Acc, <<H, T/binary>>) when H >= $0, H =< $9 ->
  decode_string([H | Acc], T);
decode_string(_Acc, <<_Data/binary>>) ->
  {error, badarg}.

reduce_string(_Acc, Times, <<>>) when Times > 0 ->
  {error, badarg};
reduce_string(Acc, 0, <<Data/binary>>) ->
  {ok, list_to_binary(lists:reverse(Acc)), Data};
reduce_string(Acc, Times, <<H, T/binary>>) ->
  reduce_string([H | Acc], Times - 1, T).

decode_integer(Acc, <<"e", T/binary>>) ->
  {ok, list_to_integer(lists:reverse(Acc)), T};
decode_integer(Acc, <<H, T/binary>>) ->
  decode_integer([H | Acc], T);
decode_integer(_Acc, <<>>) ->
  {error, badarg};
decode_integer(_Acc, <<_Data/binary>>) ->
  {error, badarg}.
