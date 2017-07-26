module AST = Common.AST;

module Eval = Evaluate.Eval;

module Builtins = BuiltinFuncs.Builtins BotEnv;

module StringMap = Common.StringMap;

let baseSymbols = Builtins.add_builtins Eval.empty;

let process_input ::uuidMap ::symbolTable ::cb ::db ::threadid ::input :unit =>
  SqlHelper.get_stdlib_usertable
    db
    threadid
    ::symbolTable
    uuidToNodeMap::uuidMap
    (
      fun (userTable, uuidToNodeMap) => {
        let state = {
          Common.EvalState.userTable: userTable,
          symbolTable,
          uuidToNodeMap,
          addedUuids: []
        };
        let state = BotBuiltins.add_threadid_to_builtins threadid state;
        switch (Parse.Parser.parse_single input) {
        | Ok e =>
          Eval.eval
            e
            ctx::(Eval.create_initial_context state)
            ::state
            cb::(
              fun (r, s) =>
                SqlHelper.put_usertable
                  db threadid s.userTable (fun _ => cb (r, s.uuidToNodeMap))
            )
        | Error _ as e => cb (e, state.uuidToNodeMap)
        }
      }
    );

Random.self_init ();

let db = Sqlite.database ":memory:";

let creds = ChatApi.parseLoginCreds "login.json";

let global_uuidmap = ref None;

let listen_cb api symbolTable _err maybe_msg =>
  switch (Js.Null.to_opt maybe_msg) {
  | None => failwith "Error in msg from listen"
  | Some msg =>
    let m = ChatApi.getBody msg;
    let threadid = ChatApi.getThreadID msg;
    let msgid = ChatApi.getMessageID msg;
    let (m, quiet) =
      if (m.[0] == '!' && m.[1] == '(') {
        (String.sub m 1 (String.length m - 1), false)
      } else {
        (m, true)
      };
    if (m.[0] == '(' && m.[String.length m - 1] == ')') {
      switch !global_uuidmap {
      | None => ChatApi.setMessageReaction api emote::":sad:" id::msgid
      | Some uuidMap =>
        process_input
          ::uuidMap
          ::symbolTable
          ::db
          ::threadid
          input::m
          cb::(
            fun (result, uuidMap) => {
              global_uuidmap := Some uuidMap;
              switch (result, quiet) {
              | (Error _, false)
              | (Ok _, _) =>
                ChatApi.sendMessage
                  api msg::(AST.to_string result) id::threadid
              | (Error _, true) =>
                ChatApi.setMessageReaction api emote::":dislike:" id::msgid
              }
            }
          )
      }
    }
  };

let start uuidmap => {
  global_uuidmap := Some uuidmap;
  ChatApi.login
    creds
    (
      fun _ maybe_api =>
        switch (Js.Null.to_opt maybe_api) {
        | None => failwith "Could not log in"
        | Some api =>
          let apiSymbols = (BotBuiltins.add_builtins baseSymbols api).symbolTable;
          ChatApi.listen api (listen_cb api apiSymbols)
        }
    )
};

SqlHelper.create_tables db (fun _ => SqlHelper.load_uuidmap db start);
