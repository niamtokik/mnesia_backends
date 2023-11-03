%%%===================================================================
%%% @doc 
%%%
%%% This module implement a Mnesia backend of Unix Filesystem (UFS,
%%% FFS, EXT4) as backend example. Behaviors, specifications and
%%% perhaps macros will be also created here and shared for other
%%% projects.
%%%
%%% == Database Structure ==
%%%
%%% ```
%%% mnesia_unixfs
%%% |-- table_name1
%%% |   |-- tn1_key1
%%% |   |-- tn1_key1.key
%%% |   |-- tn1_key1.meta
%%% |   |-- tn1_key2
%%% |   |-- tn1_key2.key
%%% |   |-- tn1_key2.meta
%%% |   |-- tn1_keyN
%%% |   |-- tn1_keyN.key
%%% |   `-- tn1_keyN.meta
%%% |-- table_name2
%%% |   |-- tn2_key1
%%% |   |-- tn2_key2
%%% |   `-- tn2_keyN
%%% `-- table_nameN
%%%     |-- tnN_key1
%%%     |-- tnN_key2
%%%     `-- tnN_keyN
%%% '''
%%%
%%% === Table Name ===
%%%
%%% The table name is created using an atom and must be unique.
%%%
%%% === Key Identifier ===
%%%
%%% The Key identifier is a bit complex to easily store with filename
%%% size limitation but a simple rule can be created:
%%%
%%% <ul>
%%%   <li>If the key is an `integer()`, the key will be directly set
%%%       with this value without any encoding.
%%%   </li>
%%%   <li>If the key is any other structure, a checksum must be used,
%%%       and the real identifier will be stored in the `.key' file
%%%       stored alongside the key and encoded using ETF.
%%%   </li>
%%% </ul>
%%%
%%% Note: some structures don't have guarantees their orders, in this
%%%       case, an external encoding system like `sext' could be used.
%%%
%%% === Value Store ===
%%%
%%% Stored values will be encoded using ETF as binary file.
%%%
%%% == Specific Filesystem Options ==
%%%
%%% === Ext4 (Linux) ===
%%%
%%% === XFS (Linux) ===
%%%
%%% === Btrfs (Linux) ===
%%%
%%% === FFS2 (FreeBSD) ===
%%%
%%% === UFS2 (OpenBSD) ===
%%%
%%% === Hammer (DragonFlyBSD) ===
%%%
%%% === ZFS ===
%%%
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
-export([init_backend/0]).
-export([check_definition/4]).
-export([close_table/2, create_table/2]).
-export([delete/3, delete_table/2]).
-export([fixtable/3]).
-export([index_is_consistent/3, is_index_consistent/2]).
-export([info/3]).
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
%% -export([first/2, last/2, next/3, prev/3]).

%%--------------------------------------------------------------------
%% types from lib/mnesia/src/mnesia_backend_type.erl
%%--------------------------------------------------------------------
-type nodes()   :: [node(), ...].
-type aliases() :: [atom(), ...].
-type key()     :: any().
-type tab()     :: atom().
-type type()    :: set | bag | ordered_set.
%% -type rec_name() :: atom().
%% -type proplist() :: [{atom(), any()}].
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
      Return  :: ok.

add_aliases(Aliases) -> throw(todo).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_schema(Nodes) -> Return when
      Nodes  :: nodes(),
      Return :: ok | {error, term()}.

create_schema(Nodes) ->
    create_schema(Nodes, [unixfs_copies]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_schema(Nodes, Aliases) -> Return when
      Nodes   :: nodes(),
      Aliases :: aliases(),
      Return  :: ok | {error, term()}.

create_schema(Nodes, Aliases)
  when is_list(Nodes) andalso is_list(Aliases) ->
    Backends = [{Alias, ?MODULE} || Alias <- Aliases],
    mnesia:create_schema(Nodes, [{backend_types, Backends}]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_table(Alias, Table, Properties) -> Return when
      Alias      :: alias(),
      Table      :: tab(),
      Properties :: proplists:proplist(),
      Return     :: tab().
      
create_table(Alias, Table, Properties) -> throw(todo).

%%--------------------------------------------------------------------
%% @doc Close an open and active table. This callback is called by
%% many different mnesia modules:
%%
%% <ul>
%%   <li>`mnesia_bup:delete_ext/2'</li>
%%   <li>`mnesia_checkpoint:retainer_delete/1'</li>
%%   <li>`mnesia_dumper:disc_delete_table/2'</li>
%%   <li>`mnesia_dumper:do_close/5'</li>
%%   <li>`mnesia_index:delete_transient_index/3'</li>
%%   <li>`mnesia_loader:down/2'</li>
%%   <li>`mnesia_schema:undo_prepare_op/2'</li>
%% </ul>
%% @end
%%--------------------------------------------------------------------
-spec close_table(Alias, Table) -> Return when
      Alias  :: alias(),
      Table  :: tab(),
      Return :: ok.

close_table(Alias, Table) -> ok.    

%%--------------------------------------------------------------------
%% @doc Initialize the environment for the backend. This callback is
%% called by `mnesia_schema:init_backend/2' and must return ok to
%% continue the execution and add aliases.
%%
%% @end
%%--------------------------------------------------------------------
init_backend() ->
    application:start(mnesia_unixfs),
    ok.

%%--------------------------------------------------------------------
%% @doc this callback is called by
%% `mnesia_schema:verify_external_copy/2'
%%
%% @end
%%--------------------------------------------------------------------
-spec check_definition(Alias, Table, Nodes, Properties) -> Return when
      Alias      :: alias(),
      Table      :: tab(),
      Nodes      :: nodes(),
      Properties :: proplists:proplist(),
      Return     :: ok.

check_definition(Alias, Table, Nodes, Properties) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Delete a key in an opened and active table. This callback is
%% called by:
%%
%% <ul>
%%   <li>`mnesia:delete/3'</li>
%%   <li>`mnesia_lib:do_erase/3'</li>
%%   <li>`mnesia_loader:down/2'</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(Alias, Table, Key) -> Return when
      Alias  :: alias(),
      Table  :: tab(),
      Key    :: key(),
      Return :: ok.

delete(Alias, Table, Key) -> ok.
    
%%--------------------------------------------------------------------
%% @doc Delete a table.
%%
%% <ul>
%%   <li>`mnesia_bup:delete_ext/2'</li>
%%   <li>`mnesia_checkpoint:retainer_create/5'</li>
%%   <li>`mnesia_checkpoint:retainer_delete/1'</li>
%%   <li>`mnesia_dumper:disc_delete_table/2'</li>
%%   <li>`mnesia_dumper:inset_op/5'</li>
%%   <li>`mnesia_index:delete_transient_index/3'</li>
%%   <li>`mnesia_schema:undo_prepare_op/2'</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_table(Alias, Table) -> Return when
      Alias  :: alias(),
      Table  :: tab(),
      Return :: ok.

delete_table(Alias, Table) -> ok.

%%--------------------------------------------------------------------
%% @doc this callback is called by `mnesia_lib:db_fixtable/3'.
%% @end
%%--------------------------------------------------------------------
-spec fixtable(Alias, Table, Bool) -> Return when
      Alias  :: alias(),
      Table  :: tab(),
      Bool   :: boolean(),
      Return :: ok | true.

fixtable(Alias, Table, Bool) -> ok.

%%--------------------------------------------------------------------
%% @doc called by `mnesia_index:init_ext_index/5'.
%% @end
%%--------------------------------------------------------------------
-spec index_is_consistent(TypeAlias, IndexTag, Bool) -> Return when
      TypeAlias :: any(), % not sure yet
      IndexTag  :: {Table, index, PosInfo},
      Table     :: tab(),
      PosInfo   :: {Pos, Type},
      % @TODO: Pos: don't really know yet, but it looks like it's an
      %        integer based on mnesia_schema:attr_tab_to_pos/2 
      %        function.
      Pos       :: integer(),
      Type      :: type(),
      Bool      :: boolean(),
      Return    :: ok.

index_is_consistent(TypeAlias, IndexTag, Bool) -> ok.

%%--------------------------------------------------------------------
%% @doc called by `mnesia_index:init_ext_index/5'.
%% @end
%%--------------------------------------------------------------------
-spec is_index_consistent(Alias, IndexTag) -> Return when
      Alias    :: alias(), % not sure yet
      IndexTag :: {Table, index, PosInfo},
      Table    :: tab(),
      PosInfo  :: {Pos, Type},
      % @TODO: Pos: don't really know yet, but it looks like it's an
      %        integer based on mnesia_schema:attr_tab_to_pos/2 
      %        function.
      Pos      :: integer(),
      Type     :: type(),
      Return   :: boolean().

is_index_consistent(Alias, IndexTag) -> true.
    
%%--------------------------------------------------------------------
%% @doc called by `mnesia:raw_table_info/2', `mnesia_controller:info/1
%% and `mnesia_loader:get_chunk_func/4'.
%% @end
%%--------------------------------------------------------------------
-spec info(TypeAlias, Table, Item) -> Return when
      TypeAlias :: any(), % not sure yet
      Table     :: tab(),
      Item      :: term(), % usually size | memory
      Return    :: term()

info(TypeAlias, Table, Item) -> ok.


