open Core.Std
open Async.Std

type t = {
  threads: In_thread.Helper_thread.t array;
  mutable next: int;
}

type db = {
  db: Sqlite3.db;
  thread: In_thread.Helper_thread.t;
}

type stmt = {
  stmt: Sqlite3.stmt;
  thread: In_thread.Helper_thread.t;
}

let create ?(max_sqlite_threads=4) () = {
  threads=Array.init max_sqlite_threads
    ~f:(fun _ ->
      match In_thread.Helper_thread.create ~name:"sqlite" () with
        | Ok v -> v
        | Error _ -> assert false);
  next=0;
}

let create_stmt (db : db) stmt = { stmt; thread=db.thread; }

let next_thread t =
  let thread = t.threads.(t.next) in
  t.next <- (t.next + 1) % (Array.length t.threads);
  thread

let db_open t ?mode ?mutex ?cache ?vfs path =
  let thread = next_thread t in
  In_thread.run ~thread (fun () ->
    Sqlite3.db_open ?mode ?mutex ?cache ?vfs path)
  >>| fun db ->
  { db; thread; }

let in_thread f (db : db) =
  In_thread.run ~thread:db.thread (fun () -> f db.db)

let in_thread_stmt f stmt =
  In_thread.run ~thread:stmt.thread (fun () -> f stmt.stmt)

let db_close = in_thread Sqlite3.db_close

let enable_load_extension db onoff =
  in_thread (fun db -> Sqlite3.enable_load_extension db onoff) db

let errcode = in_thread Sqlite3.errcode

let errmsg = in_thread Sqlite3.errmsg

let last_insert_rowid = in_thread Sqlite3.last_insert_rowid

let exec db ?cb sql =
  in_thread (fun db -> Sqlite3.exec db ?cb sql) db

let exec_no_headers db cb sql =
  in_thread (fun db -> Sqlite3.exec_no_headers db ~cb sql) db

let exec_not_null db cb sql =
  in_thread (fun db -> Sqlite3.exec_not_null db ~cb sql) db

let exec_not_null_no_headers db cb sql =
  in_thread (fun db -> Sqlite3.exec_not_null_no_headers db ~cb sql) db

let changes = in_thread Sqlite3.changes

let prepare db sql =
  in_thread (fun db -> Sqlite3.prepare db sql) db >>| create_stmt db

let prepare_tail stmt =
  in_thread_stmt Sqlite3.prepare_tail stmt
  >>| fun sync_stmt ->
  Option.bind sync_stmt (fun sync_stmt ->
    Some { stmt=sync_stmt; thread=stmt.thread; })

let recompile = in_thread_stmt Sqlite3.recompile

let step = in_thread_stmt Sqlite3.step

let finalize = in_thread_stmt Sqlite3.finalize

let reset = in_thread_stmt Sqlite3.reset

let data_count = in_thread_stmt Sqlite3.data_count

let column_count = in_thread_stmt Sqlite3.column_count

let column stmt i = in_thread_stmt (fun stmt -> Sqlite3.column stmt i) stmt

let column_name stmt i = in_thread_stmt (fun stmt -> Sqlite3.column_name stmt i) stmt

let column_decltype stmt i = in_thread_stmt (fun stmt -> Sqlite3.column_decltype stmt i) stmt

let bind stmt i data = in_thread_stmt (fun stmt -> Sqlite3.bind stmt i data) stmt

let bind_parameter_count = in_thread_stmt Sqlite3.bind_parameter_count

let bind_parameter_name stmt i =
  in_thread_stmt (fun stmt -> Sqlite3.bind_parameter_name stmt i) stmt

let bind_parameter_index stmt name =
  in_thread_stmt (fun stmt -> Sqlite3.bind_parameter_index stmt name) stmt

let row_data = in_thread_stmt Sqlite3.row_data

let row_names = in_thread_stmt Sqlite3.row_names

let row_decltypes = in_thread_stmt Sqlite3.row_decltypes
