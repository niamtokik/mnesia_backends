%%%===================================================================
%%% @doc
%%% @end
%%%===================================================================
-module(mnesia_unixfs).
-behavior(mnesia_backend_type).
-behavior(gen_server).
-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% callback from lib/mnesia/src/mnesia_backend_type.erl
%%--------------------------------------------------------------------
-export([add_aliases/1]).
-export([create_schema/1, create_schema/2]).
%% -export([init_backend/0]).
%% -export([check_definition/4]).
%% -export([close_table/2, create_table/2]).
%% -export([delete/3, delete_table/2]).
%% -export([fixtable/3]).
%% -export([first/2, last/2, next/3, prev/3]).
%% -export([index_is_consistent/3, is_index_consistent/2]).
%% -export([info/3]).
%% -export([insert/3]).
%% -export([lookup/3]).
%% -export([load_table/4]).
%% -export([match_delete/3]).
%% -export([receiver_first_message/4]).
%% -export([receive_data/5, eceive_done/4]).
%% -export([real_suffixes/0]).
%% -export([remove_aliases/1]).
%% -export([repair_continuation/2]).
%% -export([select/1, select/3, select/4]).
%% -export([sender_init/4]).
%% -export([semantics/2]).
%% -export([slot/3]).
%% -export([sync_close_table/2]).
%% -export([tmp_suffixes/0]).
%% -export([update_counter/4]).
%% -export([validate_key/6, validate_record/6]).

%%--------------------------------------------------------------------
%% types from lib/mnesia/src/mnesia_backend_type.erl
%%--------------------------------------------------------------------
-type nodes() :: [node(), ...].
-type aliases() :: [atom(), ...].
%% -type tab() :: atom().
%% -type rec_name() :: atom().
%% -type type() :: set | bag | ordered_set.
%% -type proplist() :: [{atom(), any()}].
%% -type key() :: any().
%% -type db_object() :: tuple().
%% -type matchspec() :: ets:match_spec().
%% -type limit() :: integer() | infinity.
%% -type cont_fun() :: any().
%% -type cont() :: '$end_of_table' | cont_fun().

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec add_aliases(Aliases) -> Return when
      Aliases :: aliases(),
      Return :: ok.

add_aliases(Aliases) -> throw(todo).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_schema(Nodes) -> Return when
      Nodes :: nodes(),
      Return :: ok | {error, term()}.

create_schema(Nodes) ->
    create_schema(Nodes, [unixfs_copies]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_schema(Nodes, Aliases) -> Return when
      Nodes :: nodes(),
      Aliases :: aliases(),
      Return :: ok | {error, term()}.

create_schema(Nodes, Aliases)
  when is_list(Nodes) andalso is_list(Aliases) ->
    Backends = [{Alias, ?MODULE} || Alias <- Aliases],
    mnesia:create_schema(Nodes, [{backend_types, Backends}]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
create_table(Alias, Table, Props) -> throw(todo).
