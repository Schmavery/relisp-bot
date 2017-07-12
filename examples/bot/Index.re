type errorT;

external readFile :
  string =>
  _ [@bs.as "utf8"] =>
  (errorT => Js.Undefined.t string => unit) =>
  unit =
  "" [@@bs.module "fs"];

module Environment: BuiltinFuncs.EnvironmentT = {
  let load_lib filename ::cb =>
    readFile
      ("stdlib/" ^ filename ^ ".lib")
      (fun _ str => cb (Js.Undefined.to_opt str));
};

let rlDef =
  Readline.createInterfaceDef input::Readline.stdin output::Readline.stdout;

let rl = Readline.createInterface rlDef;

Random.self_init ();

module AST = Common.AST;

module Eval = Evaluate.Eval;

module Builtins = BuiltinFuncs.Builtins Environment;

module StringMap = Common.StringMap;

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
      "CREATE TABLE IF NOT EXISTS Usertable (id TEXT NOT NULL PRIMARY KEY, usertable TEXT);" ^ "CREATE TABLE IF NOT EXISTS UuidMap (id TEXT NOT NULL PRIMARY KEY, node TEXT NOT NULL);"
    )
    (log_and_call cb);

let put_usertable
    db
    (id: string)
    (table: StringMap.t Common.uuidT)
    (cb: Sqlite.err => unit) =>
  Sqlite.run
    db
    "INSERT OR REPLACE INTO Usertable ('id', 'usertable') VALUES ($id, $usertable)"
    {"$id": id, "$usertable": Persist.to_string table}
    (log_and_call cb);

let get_usertable
    (db: Sqlite.t)
    (id: string)
    (cb: option (StringMap.t Common.uuidT) => unit) =>
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
    (cb: (StringMap.t Common.uuidT, StringMap.t AST.astNodeT) => unit) =>
  get_usertable
    db
    id
    (
      fun maybeTable =>
        switch maybeTable {
        | Some x => cb (x, uuidToNodeMap)
        | None =>
          /* print_endline (creating ) */
          let state = {
            Common.EvalState.symbolTable: symbolTable,
            uuidToNodeMap,
            userTable: StringMap.empty,
            addedUuids: []
          };
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
                    cb (s.userTable, s.uuidToNodeMap)
                  | Error _ =>
                    print_endline (AST.to_string res);
                    print_endline "Error evaluating stdlib, continuing...";
                    cb (s.userTable, s.uuidToNodeMap)
                  }
              )
          | Error _ as e =>
            print_endline (AST.to_string e);
            print_endline "Error parsing stdlib, continuing...";
            cb (state.userTable, state.uuidToNodeMap)
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
          switch (Common.StringMapHelper.get uuid uuidToNodeMap) {
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

let symbolTable = (Builtins.add_builtins Eval.empty).symbolTable;

let process_input
    (uuidToNodeMap: StringMap.t AST.astNodeT)
    ::cb
    db
    threadid
    in_str
    :unit =>
  get_stdlib_usertable
    db
    threadid
    ::symbolTable
    ::uuidToNodeMap
    (
      fun (userTable, uuidToNodeMap) => {
        let state = {
          Common.EvalState.userTable: userTable,
          symbolTable,
          uuidToNodeMap,
          addedUuids: []
        };
        switch (Parse.Parser.parse_single in_str) {
        | Ok e =>
          Eval.eval
            e
            ctx::(Eval.create_initial_context state)
            ::state
            cb::(
              fun (r, s) =>
                put_usertable
                  db threadid s.userTable (fun _ => cb (r, s.uuidToNodeMap))
            )
        | Error _ as e => cb (e, state.uuidToNodeMap)
        }
      }
    );

let db = Sqlite.database ":memory:";

let split_input: string => (string, string) = [%bs.raw
  {|
  function(s) {
    var firstparen = s.indexOf('(');
    var firstspace = s.indexOf(' ');
    if (firstparen == -1 || firstspace == -1 || firstspace > firstparen)
      return ["debug-", s]
    else
      return ["debug-"+s.split(' ')[0], s.split(' ').slice(1).join(' ')]
  }
|}
];

let rec prompt uuidMap =>
  Readline.question
    rl
    "> "
    (
      fun in_str => {
        let (threadid, body) = split_input in_str;
        process_input
          uuidMap
          cb::(
            fun (result, uuidMap) => {
              print_endline (AST.to_string result);
              prompt uuidMap
            }
          )
          db
          threadid
          body
      }
    );

create_tables db (fun _ => load_uuidmap db (fun uuidmap => prompt uuidmap));
