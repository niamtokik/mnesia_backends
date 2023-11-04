%%%===================================================================
%%% @doc
%%% @end
%%%===================================================================
-module(mnesia_debug).
-behavior(mnesia_backend_type).
-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% callback from lib/mnesia/src/mnesia_backend_type.erl
%%--------------------------------------------------------------------
-export([add_aliases/1]).
-export([create_schema/1, create_schema/2]).
-export([init_backend/0]).
-export([check_definition/4]).
-export([close_table/2, create_table/3]).
-export([delete/3, delete_table/2]).
-export([fixtable/3]).
-export([index_is_consistent/3, is_index_consistent/2]).
-export([info/3]).
-export([insert/3]).
-export([lookup/3]).
-export([load_table/4]).
-export([match_delete/3]).
-export([receiver_first_message/4]).
-export([receive_data/5, receive_done/4]).
-export([real_suffixes/0]).
-export([remove_aliases/1]).
-export([repair_continuation/2]).
-export([select/1, select/3, select/4]).
-export([sender_init/4]).
-export([semantics/2]).
-export([slot/3]).
-export([sync_close_table/2]).
-export([tmp_suffixes/0]).
-export([update_counter/4]).
-export([validate_key/6, validate_record/6]).
-export([first/2, last/2, next/3, prev/3]).

%%--------------------------------------------------------------------
%% types from lib/mnesia/src/mnesia_backend_type.erl
%%--------------------------------------------------------------------
-type nodes()        :: [node(), ...].
-type alias()        :: atom().
-type aliases()      :: [alias(), ...].
-type key()          :: any().
-type table()        :: atom().
-type type()         :: set | bag | ordered_set.
-type db_object()    :: tuple().
-type record_name()  :: atom().
-type cont_fun()     :: any().
-type continuation() :: '$end_of_table' | cont_fun().

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec add_aliases(Aliases) -> Return when
      Aliases :: aliases(),
      Return  :: ok.

add_aliases(Aliases) -> 
    ?LOG_DEBUG("~p",[{?MODULE, self(), add_aliases, [Aliases]}]),
    throw({todo, ?MODULE, add_aliases, [Aliases]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_schema(Nodes) -> Return when
      Nodes  :: nodes(),
      Return :: ok | {error, term()}.

create_schema(Nodes) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), create_schema, [Nodes]}]),
    throw({todo, ?MODULE, create_schema, [Nodes]}).

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
    ?LOG_DEBUG("~p",[{?MODULE, self(), create_schema, [Nodes, Aliases]}]),
    _Backends = [{Alias, ?MODULE} || Alias <- Aliases],
    throw({todo, ?MODULE, create_schema, [Nodes, Aliases]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_table(Alias, Table, Properties) -> Return when
      Alias      :: alias(),
      Table      :: table(),
      Properties :: proplists:proplist(),
      Return     :: table().
      
create_table(Alias, Table, Properties) -> 
    ?LOG_DEBUG("~p",[{?MODULE, self(), create_table, [Alias, Table, Properties]}]),
    throw({todo, ?MODULE, create_table, [Alias, Table, Properties]}).

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
      Table  :: table(),
      Return :: ok.

close_table(Alias, Table) -> 
    ?LOG_DEBUG("~p",[{?MODULE, self(), close_table, [Alias, Table]}]),
    throw({todo, ?MODULE, close_table, [Alias, Table]}).

%%--------------------------------------------------------------------
%% @doc Initialize the environment for the backend. This callback is
%% called by `mnesia_schema:init_backend/2' and must return ok to
%% continue the execution and add aliases.
%%
%% @end
%%--------------------------------------------------------------------
init_backend() ->
    application:start(mnesia_debug),
    throw({todo, ?MODULE, init_backend, []}).

%%--------------------------------------------------------------------
%% @doc this callback is called by
%% `mnesia_schema:verify_external_copy/2'
%%
%% @end
%%--------------------------------------------------------------------
-spec check_definition(Alias, Table, Nodes, Properties) -> Return when
      Alias      :: alias(),
      Table      :: table(),
      Nodes      :: nodes(),
      Properties :: proplists:proplist(),
      Return     :: ok.

check_definition(Alias, Table, Nodes, Properties) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), check_definition, [Alias, Table, Nodes, Properties]}]),
    throw({todo, ?MODULE, check_definition, [Alias, Table, Nodes, Properties]}).

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
      Table  :: table(),
      Key    :: key(),
      Return :: ok.

delete(Alias, Table, Key) -> 
    ?LOG_DEBUG("~p",[{?MODULE, self(), delete, [Alias, Table, Key]}]),
    throw({todo, ?MODULE, delete, [Alias, Table, Key]}).
    
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
      Table  :: table(),
      Return :: ok.

delete_table(Alias, Table) -> 
    ?LOG_DEBUG("~p",[{?MODULE, self(), delete_table, [Alias, Table]}]),
    throw({todo, ?MODULE, delete_table, [Alias, Table]}).

%%--------------------------------------------------------------------
%% @doc this callback is called by `mnesia_lib:db_fixtable/3'.
%% @end
%%--------------------------------------------------------------------
-spec fixtable(Alias, Table, Bool) -> Return when
      Alias  :: alias(),
      Table  :: table(),
      Bool   :: boolean(),
      Return :: ok | true.

fixtable(Alias, Table, Bool) -> 
    ?LOG_DEBUG("~p",[{?MODULE, self(), fixtable, [Alias, Table, Bool]}]),
    throw({todo, ?MODULE, fixtable, [Alias, Table, Bool]}).

%%--------------------------------------------------------------------
%% @doc called by `mnesia_index:init_ext_index/5'.
%% @end
%%--------------------------------------------------------------------
-spec index_is_consistent(TypeAlias, IndexTag, Bool) -> Return when
      TypeAlias :: any(), % not sure yet
      IndexTag  :: {Table, index, PosInfo},
      Table     :: table(),
      PosInfo   :: {Pos, Type},
      % @TODO: Pos: don't really know yet, but it looks like it's an
      %        integer based on mnesia_schema:attr_tab_to_pos/2 
      %        function.
      Pos       :: integer(),
      Type      :: type(),
      Bool      :: boolean(),
      Return    :: ok.

index_is_consistent(TypeAlias, IndexTag, Bool) -> 
    ?LOG_DEBUG("~p",[{?MODULE, self(), index_is_consistent, [TypeAlias, IndexTag, Bool]}]),
    throw({todo, ?MODULE, index_is_consistent, [TypeAlias, IndexTag, Bool]}).

%%--------------------------------------------------------------------
%% @doc called by `mnesia_index:init_ext_index/5'.
%% @end
%%--------------------------------------------------------------------
-spec is_index_consistent(Alias, IndexTag) -> Return when
      Alias    :: alias(), % not sure yet
      IndexTag :: {Table, index, PosInfo},
      Table    :: table(),
      PosInfo  :: {Pos, Type},
      % @TODO: Pos: don't really know yet, but it looks like it's an
      %        integer based on mnesia_schema:attr_tab_to_pos/2 
      %        function.
      Pos      :: integer(),
      Type     :: type(),
      Return   :: boolean().

is_index_consistent(Alias, IndexTag) -> 
    ?LOG_DEBUG("~p",[{?MODULE, self(), is_index_consistent, [Alias, IndexTag]}]),
    throw({todo, ?MODULE, is_index_consistent, [Alias, IndexTag]}).
    
%%--------------------------------------------------------------------
%% @doc called by `mnesia:raw_table_info/2', `mnesia_controller:info/1
%% and `mnesia_loader:get_chunk_func/4'.
%% @end
%%--------------------------------------------------------------------
-spec info(TypeAlias, Table, Item) -> Return when
      TypeAlias :: alias(), % not sure yet
      Table     :: table(),
      Item      :: term(), % usually size | memory
      Return    :: term().

info(TypeAlias, Table, Item) -> 
    ?LOG_DEBUG("~p",[{?MODULE, self(), into, [TypeAlias, Table, Item]}]),
    throw({todo, ?MODULE, info, [TypeAlias, Table, Item]}).

%%--------------------------------------------------------------------
%% @doc Called by 
%%
%% <ul>
%%   <li>`mnesia_checkpoint:retainer_put/2'</li>
%%   <li>`mnesia_index:init_ext_index/5'</li>
%%   <li>`mnesia_lib:db_put/3'</li>
%%   <li>`mnesia_loader:db_put/2'</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec insert(TypeAlias, Table, Object) -> Result when
      TypeAlias :: alias(),
      Table     :: table(),
      Object    :: db_object(),
      Result    :: ok.

insert(TypeAlias, Table, Object) -> 
    ?LOG_DEBUG("~p",[{?MODULE, self(), insert, [TypeAlias, Table, Object]}]),
    throw({todo, ?MODULE, insert, [TypeAlias, Table, Object]}).

%%--------------------------------------------------------------------
%% @doc Called by
%%
%% <ul>
%%   <li>`mnesia_checkpoint:retainer_get/2'</li>
%%   <li>`mnesia_lib:db_get/3'</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec lookup(TypeAlias, Table, Key) -> Result when 
      TypeAlias :: alias(),
      Table :: table(),
      Key :: key(),
      Result :: [Objects],
      Objects :: db_object().

lookup(TypeAlias, Table, Key) -> 
    ?LOG_DEBUG("~p",[{?MODULE, self(), lookup, [TypeAlias, Table, Key]}]),
    throw({todo, ?MODULE, lookup, [TypeAlias, Table, Key]}).

%%--------------------------------------------------------------------
%% @doc Called by
%%
%% <ul>
%%   <li>`mnesia_checkpoint:retainer_create/5'</li>
%%   <li>`mnesia_dumper:insert_op/5'</li>
%%   <li>`mnesia_dumper:open_files/5'</li>
%%   <li>`mnesia_index'</li>
%%   <li>`mnesia_loader'</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec load_table(TypeAlias, Table, Reason, CsList) -> Return when
      TypeAlias :: alias(),
      Table :: table(),
      Reason :: term(),
      CsList :: any(), % to be defined
      Return :: ok.

load_table(TypeAlias, Table, Reason, CsList) -> 
    ?LOG_DEBUG("~p",[{?MODULE, self(), load_table, [TypeAlias, Table, Reason, CsList]}]),
    throw({todo, ?MODULE, load_table, [TypeAlias, Table, Reason, CsList]}).

%%--------------------------------------------------------------------
%% @doc
%%
%% <ul>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec match_delete(TypeAlias, Table, Pattern) -> Return when
      TypeAlias :: alias(),
      Table :: table(),
      Pattern :: MatchSpec,
      MatchSpec :: ets:match_spec(),
      Return :: ok.

match_delete(TypeAlias, Table, Pattern) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), load_table, [TypeAlias, Table, Pattern]}]),
    throw({todo, ?MODULE, match_delete, [TypeAlias, Table, Pattern]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec receiver_first_message(Sender, FirstMsg, Alias, Table) -> Return when
      Sender :: pid(),
      FirstMsg :: {first, term()},
      Alias :: alias(),
      Table :: table(),
      Return :: {Size, State},
      Size :: integer(), % not sure yet
      State :: any().

receiver_first_message(Sender, FirstMsg, Alias, Table) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), receiver_first_message, [Sender, FirstMsg, Alias, Table]}]),
    throw({todo, ?MODULE, receiver_first_message, [Sender, FirstMsg, Alias, Table]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec receive_data(Data, Alias, Name, Sender, State) -> Return when
      Data :: term(),
      Alias :: alias(),
      Name :: any(), % not sure yet
      Sender :: pid(),
      State :: any(), % not sure yet
      Return :: {more, State}
              | {{more, Message}, State},
      Message :: any().

receive_data(Data, Alias, Name, Sender, State) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), receive_data, [Data, Alias, Name, Sender, State]}]),
    throw({todo, ?MODULE, receive_data, [Data, Alias, Name, Sender, State]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec receive_done(Alias, Table, Sender, State) -> Return when
      Alias :: alias(),
      Table :: table(),
      Sender :: pid(),
      State :: any(), % not sure yet
      Return :: ok.

receive_done(Alias, Table, Sender, State) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), receive_done, [Alias, Table, Sender, State]}]),
    throw({todo, ?MODULE, receive_done, [Alias, Table, Sender, State]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec real_suffixes() -> Return when
      Return :: [FileSuffix],
      FileSuffix :: any(). % not sure yet

real_suffixes() -> 
    ?LOG_DEBUG("~p",[{?MODULE, self(), real_suffixes, []}]),
    throw({todo, ?MODULE, real_suffixes, []}).
    
%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec remove_aliases(Aliases) -> Return when
      Aliases :: [alias()],
      Return :: ok.

remove_aliases(Aliases) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), remove_aliases, [Aliases]}]),
    throw({todo, ?MODULE, remove_aliases, [Aliases]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec repair_continuation(Continuation, MatchSpec) -> Return when
      Continuation :: continuation(),
      MatchSpec :: ets:match_spec(),
      Return :: continuation().

repair_continuation(Continuation, MatchSpec) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), repair_continuation, [Continuation, MatchSpec]}]),
    throw({todo, ?MODULE, repair_continuation, [Continuation, MatchSpec]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec select(Continuation) -> Return when
      Continuation :: continuation(),
      Return :: {[Match], Continuation}
              | '$end_of_table',
      Match :: term().

select(Continuation) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), select, [Continuation]}]),
    throw({todo, ?MODULE, select, [Continuation]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec select(TypeAlias, Table, Pattern) -> Return when
      TypeAlias :: alias(),
      Table :: table(),
      Pattern :: ets:match_pattern(),
      Return :: {[Match], continuation()}
              | '$end_of_table',
      Match :: term().

select(TypeAlias, Table, Pattern) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), select, [TypeAlias, Table, Pattern]}]),
    throw({todo, ?MODULE, select, [TypeAlias, Table, Pattern]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec select(TypeAlias, Table, MatchSpec, Limit) -> Return when
      TypeAlias :: alias(),  
      Table :: table(),
      MatchSpec :: ets:match_spec(),
      Limit :: integer | infinity,
      Return :: {list(), continuation()}.

select(TypeAlias, Table, MatchSpec, Limit) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), select, [TypeAlias, Table, MatchSpec, Limit]}]),
    throw({todo, ?MODULE, select, [TypeAlias, Table, MatchSpec, Limit]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec sender_init(TypeAlias, Table, LoadReason, Pid) -> Return when
      TypeAlias :: alias(),
      Table :: table(),
      LoadReason :: any(), % not sure yet
      Pid :: pid(),
      Return ::  {standard, Init, Chunk} 
               | {Init, Chunk},
      Init :: any(), % not sure yet
      Chunk :: any(). % not sure yet

sender_init(TypeAlias, Table, LoadReason, Pid) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), sender_init, [TypeAlias, Table, LoadReason, Pid]}]),
    throw({todo, ?MODULE, sender_init, [TypeAlias, Table, LoadReason, Pid]}).
      
%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec semantics(TypeAlias, Item) -> Return when
      TypeAlias :: alias(),
      Item :: storage | types | index_fun | index_types,
      Return :: ram_copies | disc_copies
              | set | ordered_set | bag 
              | function() | ordered | bag.

semantics(TypeAlias, Item) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), semantics, [TypeAlias, Item]}]),
    throw({todo, ?MODULE, semantics, [TypeAlias, Item]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec slot(TypeAlias, Table, Pos) -> Return when
      TypeAlias :: alias(),
      Table :: table(),
      Pos :: integer(),
      Return :: '$end_of_table' 
              | Objects 
              | {error, Reason},
      Objects :: [db_object()],
      Reason :: any().

slot(TypeAlias, Table, Pos) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), slot, [TypeAlias, Table, Pos]}]),
    throw({todo, ?MODULE, slot, [TypeAlias, Table, Pos]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec sync_close_table(TypeAlias, Table) -> Return when
      TypeAlias :: alias(),
      Table :: table(),
      Return :: ok.

sync_close_table(TypeAlias, Table) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), sync_close_table, [TypeAlias, Table]}]),
    throw({todo, ?MODULE, sync_close_table, [TypeAlias, Table]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec tmp_suffixes() -> Return when
      Return :: [FileSuffix],
      FileSuffix :: any(). % not sure yet.

tmp_suffixes() ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), tmp_suffixes, []}]),
    throw({todo, ?MODULE, tmp_suffixes, []}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_counter(TypeAlias, Table, Counter, Value) -> Return when
      TypeAlias :: alias(),
      Table :: table(),
      Counter :: key(),
      Value :: integer(),
      Return :: integer().

update_counter(TypeAlias, Table, Counter, Value) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), update_counter, [TypeAlias, Table, Counter, Value]}]),
    throw({todo, ?MODULE, update_counter, [TypeAlias, Table, Counter, Value]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_key(Alias, Table, RecordName, Arity, Type, Key) -> Return when
      Alias :: alias(),
      Table :: table(),
      RecordName :: record_name(),
      Arity :: arity(),
      Type :: type(),
      Key :: key(),
      Return :: {record_name(), arity(), type()}.

validate_key(Alias, Table, RecordName, Arity, Type, Key) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), validate_key, [Alias, Table, RecordName, Arity, Type, Key]}]),
    throw({todo, ?MODULE, validate_key, [Alias, Table, RecordName, Arity, Type, Key]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_record(Alias, Table, RecordName, Arity, Type, Object) -> Return when
      Alias :: alias(),
      Table :: table(),
      RecordName :: record_name(),
      Arity :: arity(),
      Type :: type(),
      Object :: db_object(),
      Return :: {record_name(), arity(), type()}.

validate_record(Alias, Table, RecordName, Arity, Type, Object) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), validate_record, [Alias, Table, RecordName, Arity, Type, Object]}]),
    throw({todo, ?MODULE, validate_record, [Alias, Table, RecordName, Arity, Type, Object]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec first(TypeAlias, Table) -> Return when
      TypeAlias :: alias(),
      Table :: table(),
      Return :: term() | '$end_of_table'.

first(TypeAlias, Table) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), first, [TypeAlias, Table]}]),
    throw({todo, ?MODULE, first, [TypeAlias, Table]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec last(TypeAlias, Table) -> Return when
      TypeAlias :: alias(),
      Table :: table(),
      Return :: term() | '$end_of_table'.

last(TypeAlias, Table) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), last, [TypeAlias, Table]}]),
    throw({todo, ?MODULE, last, [TypeAlias, Table]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec next(Alias, Table, Key) -> Return when
      Alias :: alias(),
      Table :: table(),
      Key :: key(),
      Return :: term() | '$end_of_table'.

next(Alias, Table, Key) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), next, [Alias, Table, Key]}]),
    throw({todo, ?MODULE, next, [Alias, Table, Key]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec prev(Alias, Table, Key) -> Return when
      Alias :: alias(),
      Table :: table(),
      Key :: key(),
      Return :: term() | '$end_of_table'.

prev(Alias, Table, Key) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), prev, [Alias, Table, Key]}]),
    throw({todo, ?MODULE, prev, [Alias, Table, Key]}).
