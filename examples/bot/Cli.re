module AST = Common.AST;

module Eval = Evaluate.Eval;

module Builtins = BuiltinFuncs.Builtins BotEnv;

module StringMap = Common.StringMap;

let symbolTable = (Builtins.add_builtins Eval.empty).symbolTable;

let process_input
    (uuidToNodeMap: StringMap.t AST.astNodeT)
    refMap
    ::cb
    db
    threadid
    in_str
    :unit =>
  SqlHelper.get_stdlib_usertable
    db
    threadid
    ::symbolTable
    ::uuidToNodeMap
    ::refMap
    (
      fun state =>
        switch (Parse.Parser.parse_single in_str) {
        | Ok e =>
          Eval.eval
            e
            ctx::(Eval.create_initial_context state)
            ::state
            cb::(
              fun (r, s) =>
                SqlHelper.put_usertable
                  db
                  threadid
                  s.userTable
                  (
                    fun _ => cb (AST.to_string r state, s.uuidToNodeMap, refMap)
                  )
            )
        | Error _ as e =>
          cb (AST.to_string e state, state.uuidToNodeMap, refMap)
        }
    );

let split_input s => {
  let paren = String.index s '(';
  let space = String.index s ' ';
  if (paren == (-1) || space == (-1) || space > paren) {
    ("debug-", s)
  } else {
    (
      "debug-" ^ String.sub s 0 space,
      String.sub s space (String.length s - space)
    )
  }
};

let rlDef =
  Readline.createInterfaceDef input::Readline.stdin output::Readline.stdout;

let rl = Readline.createInterface rlDef;

Random.self_init ();

let db = Sqlite.database ":memory:";

let rec prompt uuidMap refMap =>
  Readline.question
    rl
    "> "
    (
      fun in_str => {
        let (threadid, body) = split_input in_str;
        process_input
          uuidMap
          refMap
          cb::(
            fun (result_str, uuidMap, refMap) => {
              print_endline result_str;
              prompt uuidMap refMap
            }
          )
          db
          threadid
          body
      }
    );

SqlHelper.create_tables
  db
  (
    fun _ =>
      SqlHelper.load_uuidmap
        db (fun uuidmap => SqlHelper.load_refmap db (prompt uuidmap))
  );
