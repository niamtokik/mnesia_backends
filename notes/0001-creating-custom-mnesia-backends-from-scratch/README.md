---
date: 2023-11-04
title: Mnesia Backends
subtitle: The Missing Documentation and Procedures to Create Custom Mnesia Backends 
author: Mathieu Kerjouan
keywords: [erlang,otp,mnesia,backends,backend,database]
license: CC BY-NC-ND
abstract: 
---

# Mnesia Backends

## The Missing Documentation and Procedures to Create Custom Mnesia Backends

Created in 1995, Mnesia is a distributed database available on each
Erlang/OTP release. It is used on many big project, like Whatsapp,
eJabberd and RabbitMQ. By default, Mnesia is using `ets` and `dets` to
store data, one in memory, and the last one on a disc. Unfortunately,
`dets` has limitations, each table can't be greater than 2GB, and it
has a real impact to use it in production environment for data
persistance.

## Introduction

Mnesia supports different kind of backends and offers also a way to
create new ones using the `mnesia_backend_type` behavior. It means
Mnesia frontend, using `qlc` for example, can be used in many context
without impacting an application already using Mnesia...

## Using Mnesia

 - [ ] creating a schema
 - [ ] creating table
 - [ ] explaining records

Mnesia, likes any other databases, is hard to understand and use, even
more when this one is coming directly from Datalog world.

## Mnesia Custom Backend Steps

### Table Creation Process

 1. A schema can be created using `mnesia:create_schema/0`
 
```erlang
mnesia:create_schema().
```
 
 2. Mnesia must be started using `mnesia:start/0`, be careful though,
    if you already have a schema on your system, it will load it, and
    it your custom backend is not present, Mnesia will panic.

```erlang
mnesia:start().
```

 3. Mnesia backend must be added using `mnesia:add_backend_type/2` or
    `mnesia_schema:add_backend_type/2` functions. It will start
    automatically the application if needed. These functions are not
    documented though.

```erlang
Alias = custom_mnesia_backend.
Module = custom_mnesia_module.
mnesia:add_backend_type(Alias, Module).
```

 4. Mnesia will call `CustomBackend:init_backend/0` function callback.

 5. Mnesia will then call `CustomBackend:add_aliases/1`, with the list
    of alias used by this module. An alias is simply another name for
    a backend, defined as an `atom()`.
 
 6. The new backend can now be seen when executing
    `mnesia:system_info/1` function with `backend_types` argument.
 
 7. A new table can now be created using `mnesia:create_table/2`
    function. To explicitly say to mnesia to use another storage, the
    name of the alias with a list of node, followed by
    `storage_properties` must be set. Mnesia will call
    `CustomModule:semantics/2` function callback.

```erlang
Opts = [],
Storage = [{Module, Opts],
mnesia:create_table( my_table
                   , [{custom_mnesia_backend, [node()]}
                   , {storage_properties, Storage}]).
```

 8. Mnesia will call then `CustomModule:check_definition/4` function
    callback and should check the properties of the table.

 9. Then `Module:create_table/3` is called

 10. `Module:semantics/2`

 11. `Module:load_table/4`

### Read Operations 

Those operations are not altering the database content.

#### `mnesia:read/2`

 1. `Module:lookup/3`

#### `mnesia:first/1`

 1. `Module:first/2`

#### `mnesia:last/1`

 1. `Module:last/2`

#### `mnesia:prev/2`

 1. `Module:prev/3`

#### `mnesia:next/2`

 1. `Module:next/3`

#### `mnesia:all_keys/1`

 1. `Module:fixtable/3`

#### `mnesia:select/2`

 1. `Module:fixtable/3`

### Write Operations 

Those operations are modifying database's content by creating,
updating or deleting one or more entries.

#### `mnesia:write/1`

 1. `Module:validate_record/6`

#### `mnesia:delete/1`

 1. `Module:delete/3`

#### `mnesia:delete_object/1`

 1. `Module:load_table/3`

## Backend Definition and Interfaces

### `Module:init_backend/0`

### `Module:add_aliases/1`

Example of debugging message:

```erlang
{ mnesia_debug
, <0.1032.0>
, add_aliases
, [[mnesia_backend_debug]
  ]
}
```

### `Module:semantics/2`

Based on `mnesia_rocksdb` implementation, `semantics/2` is needed to
define which kind of plugins are supported by this backend.

```erlang
-type storage() :: ram_copies | disc_copies | disc_only_copies.
-type types() :: [bag | set | ordered_set].
-type index_fun() :: fun(Alias, Tab, Pos, Object) -> [IndexxValue].

-spec semantics(_Alias, storage) -> storage();
      semantics(_Alias, types) -> types();
      semantics(_Alias, index_fun) -> index_fun();
      semantics(_Alias, any()) -> undefined.
```

Example of debugging message:

```erlang
{mnesia_debug,<0.1832.0>,semantics,[mnesia_backend_debug,index_types]}
{mnesia_debug,<0.1832.0>,semantics,[mnesia_backend_debug,storage]}
{mnesia_debug,<0.1832.0>,semantics,[mnesia_backend_debug,types]}
```

### `Module:check_definition/4`

Example of debugging message, where `t3` is the name of table:

```erlang
{ mnesia_debug
, <0.385.0>
, create_table
, [ mnesia_backend_debug
  , t3
  , [ {name,t3}
    , {type,set}
    , {ram_copies,[]}
    , {disc_copies,[]}
    , {disc_only_copies,[]}
    , {mnesia_backend_debug,[nonode@nohost]}
    , {load_order,0}
    , {access_mode,read_write}
    , {majority,false}
    , {index,[]}
    , {snmp,[]}
    , {local_content,false}
    , {record_name,t3}
    , {attributes,[key,val]}
    , {user_properties,[]}
    , {frag_properties,[]}
    , {storage_properties,[{mnesia_backend_debug,[]}]}
    , {cookie,{{1699140080964593704,-576460752303423483,1},nonode@nohost}}
    , {version,{{2,0},[]}}
    ]
]}
```

### `Module:close_table/2`

Example of debugging message, where `t3` is the name of table:

```erlang
{ mnesia_debug
, <0.1832.0>
, close_table
, [mnesia_backend_debug,t3]
}
```

Example of debugging message, where `t3` is the name of table:

### `Module:delete_table/2`

```erlang
{ mnesia_debug
, <0.2152.0>
, delete_table
 , [mnesia_backend_debug,t3]
}
```

### `Module:create_table/3`

Example of debugging message, where `t3` is the name of table:

```erlang
{ mnesia_debug
, <0.385.0>
, create_table
, [ mnesia_backend_debug
  , t3
  , [ {name,t3}
    , {type,set}
    , {ram_copies,[]}
    , {disc_copies,[]}
    , {disc_only_copies,[]}
    , {mnesia_backend_debug,[nonode@nohost]}
    , {load_order,0}
    , {access_mode,read_write}
    , {majority,false}
    , {index,[]}
    , {snmp,[]}
    , {local_content,false}
    , {record_name,t3}
    , {attributes,[key,val]}
    , {user_properties,[]}
    , {frag_properties,[]}
    , {storage_properties,[{mnesia_backend_debug,[]}]}
    , {cookie,{{1699140992521145194,-576460752303422015,1},nonode@nohost}}
    , {version,{{2,0},[]}}
    ]
]}
```

### `Module:load_table/4`

Example of debugging message, where `t3` is the name of table:

```erlang
{ mnesia_debug
, <0.389.0>
, load_table
, [ mnesia_backend_debug
  , t3
  , {dumper,create_table}
  , [ {name,t3}
    , {type,set}
    , {ram_copies,[]}
    , {disc_copies,[]}
    , {disc_only_copies,[]}
    , {mnesia_backend_debug,[nonode@nohost]}
    , {load_order,0}
    , {access_mode,read_write}
    , {majority,false}
    , {index,[]}
    , {snmp,[]}
    , {local_content,false}
    , {record_name,t3}
    , {attributes,[key,val]}
    , {user_properties,[]}
    , {frag_properties,[]}
    , {storage_properties,[{mnesia_backend_debug,[]}]}
    , {cookie,{{1699140992521145194,-576460752303422015,1},nonode@nohost}}
    , {version,{{2,0},[]}}
    ]
]}
```

## Backend Example

 - [ ] starting a custom backend with `mnesia_debug` backend (within
   this project);
 - [ ] explaining mnesia interfaces;
 - [ ] creating a simple backend as directories/files with
   mnesia_unixfs;

## Unix Filesystem Backend

# FAQ

## What happens if the backend is not available and a schema has already been created with it?

Well. It crashes and one will not be allowed to restart Mnesia until
the backend is correctly started or available on the system. A core
file called `MnesiaCore.${nodename}` is created and can be analyzed
with [`crashdump_viewer`](https://www.erlang.org/doc/man/crashdump_viewer)

## How to list enabled backends?

By using `mnesia:system_info/1` function.

```erlang
mnesia:system_info(backend_types).
% [ram_copies,disc_copies,disc_only_copies,mnesia_custom_backend]
```

or by using `mnesia_schema:backend_types/0` function.

```erlang
mnesia_schema:backend_types().
% [ram_copies,disc_copies,disc_only_copies]
```

## How to delete a backend.

By using `mnesia_schema:delete_backend_type/1` function.

```erlang
mnesia_schema:delete_backend_type(mnesia_custom_backend).
```

# References and Resources
    
