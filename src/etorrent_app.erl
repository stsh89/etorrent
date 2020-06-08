%%%-------------------------------------------------------------------
%% @doc etorrent public API
%% @end
%%%-------------------------------------------------------------------

-module(etorrent_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    etorrent_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
