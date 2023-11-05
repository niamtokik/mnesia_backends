%%%===================================================================
%%% @doc
%%% @end
%%%===================================================================
-module(mnesia_debug_app).
-behavior(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mnesia_debug_sup:start_link().

stop(_State) ->
    ok.
