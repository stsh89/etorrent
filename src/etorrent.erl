-module(etorrent).
-export([start_link/0, init/1]).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  io:format("Hello World~n"),
  halt(0).
