%%%===================================================================
%%% @doc
%%% @end
%%%===================================================================
-module(mnesia_debug_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{ strategy => one_for_all
                , intensity => 0
                , period => 1
                },
    ChildSpecs = [#{ id => mnesia_debug_table
                   , start => {mnesia_debug_table, start_link, []}
                   }
                 ],
    {ok, {SupFlags, ChildSpecs}}.
