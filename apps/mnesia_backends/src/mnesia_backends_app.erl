%%%-------------------------------------------------------------------
%% @doc mnesia_backends public API
%% @end
%%%-------------------------------------------------------------------

-module(mnesia_backends_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mnesia_backends_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
