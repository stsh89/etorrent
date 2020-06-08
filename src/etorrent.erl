-module(etorrent).
-export([start_link/0, init/1]).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  Res = bencoder:decode(list_to_binary("i32234e")),
  io:write(Res),
  io:format("~n"),
  halt(0).
