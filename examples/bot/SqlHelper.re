module Eval = Evaluate.Eval;

module Builtins = BuiltinFuncs.Builtins BotEnv;

open Common;

let log_and_call cb err => {
  if (not (Js.Null.test (Js.Null.return err))) {
    Js.log err
  };
  cb err
};

let create_tables db (cb: Sqlite.err => unit) =>
  Sqlite.exec
    db
    (
      "CREATE TABLE IF NOT EXISTS Usertable (id TEXT NOT NULL PRIMARY KEY, usertable TEXT);" ^
      "CREATE TABLE IF NOT EXISTS UuidMap (id TEXT NOT NULL PRIMARY KEY, node TEXT NOT NULL);" ^
      "CREATE TABLE IF NOT EXISTS RefMap (refId TEXT NOT NULL PRIMARY KEY, uuid TEXT NOT NULL);" ^
      "CREATE TABLE IF NOT EXISTS Listen (threadId TEXT NOT NULL PRIMARY KEY, name TEXT NOT NULL, pattern TEXT NOT NULL, uuid TEXT NOT NULL);" ^ ""
    )
    (log_and_call cb);

let put_usertable
    db
    (id: string)
    (table: StringMap.t (docsT, uuidT))
    (cb: Sqlite.err => unit) =>
  Sqlite.run
    db
    "INSERT OR REPLACE INTO Usertable ('id', 'usertable') VALUES ($id, $usertable)"
    {"$id": id, "$usertable": Persist.to_string table}
    (log_and_call cb);

let get_usertable
    (db: Sqlite.t)
    (id: string)
    (cb: option (StringMap.t (docsT, uuidT)) => unit) =>
  Sqlite.get
    db
    "SELECT usertable FROM Usertable WHERE id = $id"
    {"$id": id}
    (
      fun _ row =>
        cb (
          switch (Js.Undefined.to_opt row) {
          | None => None
          | Some row => Some (Persist.from_string row##usertable)
          }
        )
    );

let get_stdlib_usertable
    db
    id
    ::symbolTable
    ::uuidToNodeMap
    ::refMap
    (cb: AST.evalStateT => unit) =>
  get_usertable
    db
    id
    (
      fun maybeTable => {
        let state = {
          EvalState.symbolTable: symbolTable,
          uuidToNodeMap,
          userTable: StringMap.empty,
          refMap,
          recentActions: []
        };
        switch maybeTable {
        | Some _ => cb state
        | None =>
          switch (Parse.Parser.parse_single "(load \"std\")") {
          | Ok e =>
            Eval.eval
              e
              ctx::(Eval.create_initial_context state)
              ::state
              cb::(
                fun (res, s) =>
                  switch res {
                  | Ok _ =>
                    print_endline "Stdlib autoloaded successfully.";
                    cb s
                  | Error _ =>
                    print_endline (Stringify.string_of_ast res state);
                    print_endline "Error evaluating stdlib, continuing...";
                    cb s
                  }
              )
          | Error _ as e =>
            print_endline (Stringify.string_of_ast e state);
            print_endline "Error parsing stdlib, continuing...";
            cb state
          }
        }
      }
    );

let load_uuidmap (db: Sqlite.t) (cb: StringMap.t AST.astNodeT => unit) =>
  Sqlite.all
    db
    "SELECT id, node FROM UuidMap"
    (Js.Obj.empty ())
    (
      fun _ rows =>
        cb (
          switch (Js.Undefined.to_opt rows) {
          | None => StringMap.empty
          | Some rows =>
            Array.fold_left
              (fun map [|k, v|] => StringMap.add k (Persist.from_string v) map)
              StringMap.empty
              rows
          }
        )
    );

let load_refmap (db: Sqlite.t) (cb: StringMap.t uuidT => unit) =>
  Sqlite.all
    db
    "SELECT refId, uuid FROM RefMap"
    (Js.Obj.empty ())
    (
      fun _ rows =>
        cb (
          switch (Js.Undefined.to_opt rows) {
          | None => StringMap.empty
          | Some rows =>
            Array.fold_left
              (fun map [|k, v|] => StringMap.add k v map) StringMap.empty rows
          }
        )
    );

let load_listen
    (db: Sqlite.t)
    (cb: StringMap.t (StringMap.t (string, uuidT)) => unit) =>
  Sqlite.all
    db
    "SELECT threadId, name, pattern, uuid FROM RefMap"
    (Js.Obj.empty ())
    (
      fun _ rows =>
        cb (
          switch (Js.Undefined.to_opt rows) {
          | None => StringMap.empty
          | Some rows =>
            Array.fold_left
              (
                fun map [|threadId, name, pattern, uuid|] =>
                  Common.StringMapHelper.update_default
                    threadId
                    map
                    default::StringMap.empty
                    (fun v => StringMap.add name (pattern, uuid) v)
              )
              StringMap.empty
              rows
          }
        )
    );

let add_listen
    db
    (listens: StringMap.t (StringMap.t (string, uuidT)))
    (threadId: string)
    (name: string)
    (pattern: string)
    (uuid: uuidT)
    (cb: StringMap.t (StringMap.t (string, uuidT)) => unit) =>
  Sqlite.run
    db
    "INSERT INTO Listen ('threadId', 'name', 'pattern', 'uuid') VALUES ($1, $2, $3, $4)"
    {"$1": threadId, "$2": name, "$3": pattern, "$4": uuid}
    (
      fun _ =>
        cb (
          Common.StringMapHelper.update_default
            threadId
            listens
            default::StringMap.empty
            (fun v => StringMap.add name (pattern, uuid) v)
        )
    );

let remove_listen
    db
    (listens: StringMap.t (StringMap.t (string, uuidT)))
    (threadId: string)
    (name: string)
    (cb: StringMap.t (StringMap.t (string, uuidT)) => unit) =>
  Sqlite.run
    db
    "DELETE FROM Listen WHERE threadId = $1 AND name = $2"
    {"$1": threadId, "$2": name}
    (
      fun _ =>
        cb (
          Common.StringMapHelper.update_default
            threadId
            listens
            default::StringMap.empty
            (fun v => StringMap.remove name v)
        )
    );

let ignore_first cb _ => cb ();

let rec insert_multiple
        db
        (queries: list (string, Js.t 'a))
        (cb: unit => unit)
        () =>
  switch queries {
  | [] => cb ()
  | [(query, params), ...tl] =>
    Sqlite.run db query params (ignore_first (insert_multiple db tl cb))
  };

let process_actions
    ::db
    state::(state: Common.AST.evalStateT)
    (cb: unit => unit) => {
  let queries =
    List.map
      (
        fun (action: Common.EvalState.actionT) =>
          switch action {
          | AddUuid uuid =>
            switch (StringMapHelper.get uuid state.uuidToNodeMap) {
            | None => assert false
            | Some node => (
                "INSERT INTO UuidMap ('id', 'node') VALUES ($1, $2)",
                {"$1": uuid, "$2": Persist.to_string node}
              )
            }
          | UpdateRef (refId, uuid) => (
              "INSERT INTO RefMap ('refId', 'uuid') VALUES ($1, $2)",
              {"$1": refId, "$2": Persist.to_string uuid}
            )
          }
      )
      state.recentActions;
  insert_multiple db queries cb ()
};
