module AST = Common.AST;

module Eval = Evaluate.Eval;

module Builtins = BuiltinFuncs.Builtins BotEnv;

module StringMap = Common.StringMap;

let baseSymbols = Builtins.add_builtins Eval.empty;

let persist_data db threadid (state: Common.AST.evalStateT) ::cb =>
  SqlHelper.put_usertable
    db
    threadid
    state.userTable
    (fun _ => SqlHelper.process_actions ::db ::state cb);

let process_input
    ::uuidMap
    ::refMap
    ::symbolTable
    ::cb
    ::db
    ::threadid
    ::input
    :unit =>
  SqlHelper.get_stdlib_usertable
    db
    threadid
    ::symbolTable
    uuidToNodeMap::uuidMap
    ::refMap
    (
      fun state => {
        let state = BotBuiltins.add_threadid_to_builtins threadid state;
        switch (Parse.Parser.parse_single input) {
        | Ok e =>
          Eval.eval
            e
            ctx::(Eval.create_initial_context state)
            ::state
            cb::(
              fun (r, s) =>
                persist_data
                  db
                  threadid
                  s
                  cb::(
                    fun _ =>
                      cb (
                        r,
                        AST.to_string r state,
                        (s.uuidToNodeMap, s.refMap)
                      )
                  )
            )
        | Error _ as e =>
          cb (e, AST.to_string e state, (state.uuidToNodeMap, state.refMap))
        }
      }
    );

Random.self_init ();

let db = Sqlite.database ":memory:";

let creds = ChatApi.parseLoginCreds "login.json";

let global_data = ref None;

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
      switch !global_data {
      | None => ChatApi.setMessageReaction api emote::":sad:" id::msgid
      | Some (uuidMap, refMap) =>
        process_input
          ::uuidMap
          ::refMap
          ::symbolTable
          ::db
          ::threadid
          input::m
          cb::(
            fun (result, msg, uuidMap) => {
              global_data := Some uuidMap;
              switch (result, quiet) {
              | (Error _, false)
              | (Ok _, _) => ChatApi.sendMessage api ::msg id::threadid
              | (Error _, true) =>
                ChatApi.setMessageReaction api emote::":dislike:" id::msgid
              }
            }
          )
      }
    }
  };

let start uuidmap refMap => {
  global_data := Some (uuidmap, refMap);
  ChatApi.login
    creds
    (
      fun _ maybe_api =>
        switch (Js.Null.to_opt maybe_api) {
        | None => failwith "Could not log in"
        | Some api =>
          let apiSymbols =
            (BotBuiltins.add_builtins baseSymbols api).symbolTable;
          ChatApi.listen api (listen_cb api apiSymbols)
        }
    )
};

SqlHelper.create_tables
  db
  (
    fun _ =>
      SqlHelper.load_uuidmap
        db (fun uuidmap => SqlHelper.load_refmap db (start uuidmap))
  );
