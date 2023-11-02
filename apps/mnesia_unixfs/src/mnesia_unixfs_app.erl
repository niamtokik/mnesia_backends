%%%===================================================================
%%% @doc
%%% @end
%%%===================================================================
-module(mnesia_unixfs_app).
-behavior(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mnesia_unixfs_sup:start_link().

stop(_State) ->
    ok.
