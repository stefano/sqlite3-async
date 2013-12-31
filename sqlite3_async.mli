open Core.Std
open Async.Std

type t
type db
type stmt

val create : ?max_sqlite_threads:int -> unit -> t

val db_open :
  t ->
  ?mode:[ `NO_CREATE | `READONLY ] ->
  ?mutex:[ `FULL | `NO ] ->
  ?cache:[ `PRIVATE | `SHARED ] -> ?vfs:string -> string -> db Deferred.t
val db_close : db -> bool Deferred.t
val enable_load_extension : db -> bool -> bool Deferred.t
val errcode : db -> Sqlite3.Rc.t Deferred.t
val errmsg : db -> string Deferred.t
val last_insert_rowid : db -> int64 Deferred.t
val exec :
  db ->
  ?cb:(Sqlite3.row -> Sqlite3.headers -> unit) ->
  string -> Sqlite3.Rc.t Deferred.t
val exec_no_headers :
  db -> (Sqlite3.row -> unit) -> string -> Sqlite3.Rc.t Deferred.t
val exec_not_null :
  db ->
  (Sqlite3.row_not_null -> Sqlite3.headers -> unit) ->
  string -> Sqlite3.Rc.t Deferred.t
val exec_not_null_no_headers :
  db ->
  (Sqlite3.row_not_null -> unit) -> string -> Sqlite3.Rc.t Deferred.t
val changes : db -> int Deferred.t
val prepare : db -> string -> stmt Deferred.t
val prepare_tail : stmt -> stmt option Deferred.t
val recompile : stmt -> unit Deferred.t
val step : stmt -> Sqlite3.Rc.t Deferred.t
val finalize : stmt -> Sqlite3.Rc.t Deferred.t
val reset : stmt -> Sqlite3.Rc.t Deferred.t
val data_count : stmt -> int Deferred.t
val column_count : stmt -> int Deferred.t
val column : stmt -> int -> Sqlite3.Data.t Deferred.t
val column_name : stmt -> int -> string Deferred.t
val column_decltype : stmt -> int -> string option Deferred.t
val bind : stmt -> int -> Sqlite3.Data.t -> Sqlite3.Rc.t Deferred.t
val bind_parameter_count : stmt -> int Deferred.t
val bind_parameter_name : stmt -> int -> string option Deferred.t
val bind_parameter_index : stmt -> string -> int Deferred.t
val row_data : stmt -> Sqlite3.Data.t array Deferred.t
val row_names : stmt -> Sqlite3.headers Deferred.t
val row_decltypes : stmt -> string option array Deferred.t
