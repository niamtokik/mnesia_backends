-module(mnesia_debug_table).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1]).
-export([handle_info/2, handle_cast/2, handle_call/3]).
-include_lib("kernel/include/logger.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], [debug]).

init(_) -> 
    {ok, []}.

handle_cast(Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), handle_cast, [Message, State]}]),
    {noreply, State}.

handle_call(Message, From, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), handle_call, [Message, From, State]}]),
    {noreply, State}.
    
handle_info(Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), handle_info, [Message, State]}]),
    {noreply, State}.
