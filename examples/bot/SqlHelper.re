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
      "CREATE TABLE IF NOT EXISTS RefMap (refid TEXT NOT NULL PRIMARY KEY, uuid TEXT NOT NULL);" ^ ""
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
          addedUuids: []
        };
        switch maybeTable {
        | Some _ => cb state
        | None =>
          /* print_endline (creating ) */
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
                    print_endline (AST.to_string res state);
                    print_endline "Error evaluating stdlib, continuing...";
                    cb s
                  }
              )
          | Error _ as e =>
            print_endline (AST.to_string e state);
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
    "SELECT refid, uuid FROM RefMap"
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

let add_to_uuidmap db addedUuids uuidToNodeMap (cb: unit => unit) => {
  let queries =
    List.map
      (
        fun uuid =>
          switch (StringMapHelper.get uuid uuidToNodeMap) {
          | None => assert false
          | Some node => (
              "INSERT INTO UuidMap ('id', 'node') VALUES ($id, $node)",
              {"$id": uuid, "$node": Persist.to_string node}
            )
          }
      )
      addedUuids;
  insert_multiple db queries cb ()
};
