module AST = Common.AST;

module Eval = Evaluate.Eval;

module Builtins = BuiltinFuncs.Builtins BotEnv;

module StringMap = Common.StringMap;

module Regex = {
  type t;
  external create : string => t = "RegExp" [@@bs.new];
  external test : t => string => bool = "" [@@bs.send];
};

let baseSymbols = Builtins.add_builtins Eval.empty;

let db = Sqlite.database ":memory:";

let persist_data db threadid (state: Common.AST.evalStateT) ::cb => {
  SqlHelper.put_usertable
    db
    threadid
    state.userTable
    (fun _ => SqlHelper.process_actions ::db ::state cb)
};

let build_state ::uuidMap ::refMap ::symbolTable ::threadid ::cb =>
  SqlHelper.get_stdlib_usertable
    db
    threadid
    ::symbolTable
    uuidToNodeMap::uuidMap
    ::refMap
    (
      fun state => {
        let state = BotBuiltins.add_threadid_to_builtins threadid state;
        cb state
      }
    );

let process_input ::state ::cb ::threadid ::input :unit =>
  switch (Parse.Parser.parse_single input) {
  | Ok e =>
    Eval.eval
      e
      ctx::(Eval.create_initial_context state)
      ::state
      cb::(
        fun (r, state) =>
          switch r {
          | Error _ =>
            cb (
              r,
              Stringify.string_of_ast r state,
              (state.uuidToNodeMap, state.refMap)
            )
          | Ok _ =>
            persist_data
              db
              threadid
              state
              cb::(
                fun _ =>
                  cb (
                    r,
                    Stringify.string_of_ast r state,
                    (state.uuidToNodeMap, state.refMap)
                  )
              )
          }
      )
  | Error _ as e =>
    cb (
      e,
      Stringify.string_of_ast e state,
      (state.uuidToNodeMap, state.refMap)
    )
  };

Random.self_init ();

let creds = ChatApi.parseLoginCreds "login.json";

let global_data = ref None;

let global_listens = ref StringMap.empty;

let rec process_listens
        ::msg
        ::threadid
        (remaining_listens: list (string, (string, Common.uuidT)))
        (state: AST.evalStateT)
        cb::(cb: AST.evalStateT => unit) =>
  switch remaining_listens {
  | [] => cb state
  | [(name, (pattern, uuid)), ...rest] =>
    switch (Common.StringMapHelper.get uuid state.uuidToNodeMap) {
    | None => persist_data db threadid state cb::(fun _ => cb state)
    | Some f when Regex.test (Regex.create pattern) msg =>
      Eval.eval_lambda
        f
        args::[Str msg]
        func_name::("pattern: " ^ name)
        ctx::(Eval.create_initial_context state)
        ::state
        cb::(
          fun (_, state) => process_listens ::msg ::threadid rest state ::cb
        )
    | Some _ => process_listens ::msg ::threadid rest state ::cb
    }
  };

let listen_cb api symbolTable _err maybe_msg =>
  switch (Js.Null.to_opt maybe_msg, !global_data) {
  | (None, _) => failwith "Error in msg from listen"
  | (_, None) => print_endline "Too busy"; () /* TODO: queuing */
  | (Some msg, Some (uuidMap, refMap)) =>
    let msg_body = ChatApi.getBody msg;
    let threadid = ChatApi.getThreadID msg;
    let msgid = ChatApi.getMessageID msg;
    let (m, quiet) =
      if (msg_body.[0] == '!' && msg_body.[1] == '(') {
        (String.sub msg_body 1 (String.length msg_body - 1), false)
      } else {
        (msg_body, true)
      };
    if (m.[0] == '(' && m.[String.length m - 1] == ')') {
      build_state
        ::uuidMap
        ::refMap
        ::symbolTable
        ::threadid
        cb::(
          fun state =>
            process_input
              ::state
              ::threadid
              input::m
              cb::(
                fun (result, msg, uuidAndRef) => {
                  global_data := Some uuidAndRef;
                  switch (result, quiet) {
                  | (Error _, false)
                  | (Ok _, _) => ChatApi.sendMessage api ::msg id::threadid
                  | (Error _, true) =>
                    ChatApi.setMessageReaction api emote::":dislike:" id::msgid
                  }
                }
              )
        );
      ()
    } else {
      let matching =
        List.filter
          (fun (_, (pattern, _)) => Regex.test (Regex.create pattern) msg_body)
          (
            StringMap.bindings (
              Common.StringMapHelper.get_default
                threadid !global_listens StringMap.empty
            )
          );
      switch matching {
      | [] => ()
      | _ =>
        build_state
          ::uuidMap
          ::refMap
          ::symbolTable
          ::threadid
          cb::(
            fun state =>
              process_listens
                msg::msg_body
                ::threadid
                matching
                state
                cb::(
                  fun state =>
                    global_data := Some (state.uuidToNodeMap, state.refMap)
                )
          )
      }
    }
  };

let start uuidmap refMap listens => {
  global_data := Some (uuidmap, refMap);
  global_listens := listens;
  ChatApi.login
    creds
    (
      fun _ maybe_api =>
        switch (Js.Null.to_opt maybe_api) {
        | None => failwith "Could not log in"
        | Some api =>
          let apiSymbols =
            (BotBuiltins.add_builtins baseSymbols db api global_listens).
              symbolTable;
          ChatApi.listen api (listen_cb api apiSymbols)
        }
    )
};

SqlHelper.create_tables
  db
  (
    fun _ =>
      SqlHelper.load_uuidmap
        db
        (
          fun uuidmap =>
            SqlHelper.load_refmap
              db
              (fun refMap => SqlHelper.load_listen db (start uuidmap refMap))
        )
  );
