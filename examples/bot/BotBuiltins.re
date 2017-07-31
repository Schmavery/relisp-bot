open Common;

let add_builtins
    (state: AST.evalStateT)
    (db: Sqlite.t)
    api
    (listens: ref (StringMap.t (StringMap.t (string, uuidT)))) => {
  let get_threadid (state: AST.evalStateT) =>
    switch (StringMapHelper.get "Thread.id" state.symbolTable) {
    | Some (_docs, Str threadid) => Some threadid
    | Some (_, _)
    | None => None
    };
  let id_not_loaded_ex = Common.AST.create_exception "Thread.id not loaded";
  let state =
    BuiltinHelper.add_native_lambda_async
      "Thread.names"
      ::state
      macro::false
      (
        fun args ctx::_ ::state cb::return =>
          switch args {
          | [] =>
            switch (get_threadid state) {
            | Some threadid =>
              ChatApi.getThreadInfo
                api
                id::threadid
                (
                  fun _ threadInfo =>
                    switch (Js.Null.to_opt threadInfo) {
                    | None =>
                      return (
                        Common.AST.create_exception "Thread.names failed because of getThreadInfo.",
                        state
                      )
                    | Some threadInfo =>
                      ChatApi.getUserInfo
                        api
                        id::(ChatApi.getParticipantIDs threadInfo)
                        (
                          fun _ users =>
                            switch (Js.Null.to_opt users) {
                            | None =>
                              return (
                                Common.AST.create_exception "Thread.names failed because of getUserInfo.",
                                state
                              )
                            | Some users =>
                              return (
                                Ok (
                                  AST.List (
                                    List.map
                                      (fun x => AST.Str (ChatApi.getName x))
                                      (
                                        Array.to_list (
                                          ChatApi.unwrapUserInfo users
                                        )
                                      )
                                  )
                                ),
                                state
                              )
                            }
                        )
                    }
                )
            | None => return (id_not_loaded_ex, state)
            };
            ()
          | _ =>
            return (
              BuiltinHelper.received_error
                expected::0 ::args name::"Thread.names" ::state
            )
          }
      );
  let state =
    BuiltinHelper.add_native_lambda_async
      "Thread.ids"
      ::state
      macro::false
      (
        fun args ctx::_ ::state cb::return =>
          switch args {
          | [] =>
            switch (get_threadid state) {
            | Some threadid =>
              ChatApi.getThreadInfo
                api
                id::threadid
                (
                  fun _ threadInfo =>
                    switch (Js.Null.to_opt threadInfo) {
                    | None =>
                      return (
                        Common.AST.create_exception "Thread.ids failed because of getThreadInfo.",
                        state
                      )
                    | Some threadInfo =>
                      return (
                        Ok (
                          AST.List (
                            List.map
                              (fun x => AST.Str x)
                              (
                                Array.to_list (
                                  ChatApi.getParticipantIDs threadInfo
                                )
                              )
                          )
                        ),
                        state
                      )
                    }
                )
            | None => return (id_not_loaded_ex, state)
            };
            ()
          | _ =>
            return (
              BuiltinHelper.received_error
                expected::0 ::args name::"Thread.ids" ::state
            )
          }
      );
  let state =
    BuiltinHelper.add_native_lambda_async
      "Thread.send"
      ::state
      macro::false
      (
        fun args ctx::_ ::state cb::return =>
          switch args {
          | [Str id, Str msg] =>
            ChatApi.sendMessageCb
              api
              ::msg
              ::id
              (
                fun _ msgInfo =>
                  switch (Js.Null.to_opt msgInfo) {
                  | None =>
                    return (
                      Common.AST.create_exception "Thread.send failed.",
                      state
                    )
                  | Some _ => return (Ok (List []), state)
                  }
              )
          | [_, _] =>
            return (
              AST.create_exception "Expected 2 strings (id, msg) in call to Thread.send.",
              state
            )
          | _ =>
            return (
              BuiltinHelper.received_error
                expected::2 ::args name::"Thread.send" ::state
            )
          }
      );
  let state =
    BuiltinHelper.add_native_lambda
      "Listen.list"
      ::state
      macro::false
      (
        fun args ctx::_ ::state =>
          switch args {
          | [] =>
            switch (get_threadid state) {
            | Some threadid =>
              let patterns =
                StringMapHelper.get_default threadid !listens StringMap.empty;
              (
                Ok (
                  List (
                    StringMap.fold
                      (
                        fun name (pattern, _uuid) lst => [
                          AST.List [Str name, Str pattern],
                          ...lst
                        ]
                      )
                      patterns
                      []
                  )
                ),
                state
              )
            | None => (id_not_loaded_ex, state)
            }
          | _ =>
            BuiltinHelper.received_error
              expected::1 ::args name::"Listen.list" ::state
          }
      );
  let state =
    BuiltinHelper.add_native_lambda_async
      "Listen.add"
      ::state
      macro::false
      (
        fun args ctx::_ ::state cb::return =>
          switch args {
          | [Str name, Str pattern, Func _ as f]
          | [Str name, Str pattern, NativeFunc _ as f] =>
            switch (get_threadid state) {
            | Some threadid =>
              let hash = Hash.hash f;
              let state = Common.EvalState.add_to_uuidmap f hash state;
              SqlHelper.add_listen
                db
                !listens
                threadid
                name
                pattern
                hash
                (
                  fun new_listens => {
                    listens := new_listens;
                    return (Ok (List []), state)
                  }
                )
            | None => return (id_not_loaded_ex, state)
            }
          | [_, _, _] =>
            return (
              AST.create_exception "Expected 2 strings and 1 function (name, pattern, handler) in call to Listen.add.",
              state
            )
          | _ =>
            return (
              BuiltinHelper.received_error
                expected::1 ::args name::"Listen.add" ::state
            )
          }
      );
  let state =
    BuiltinHelper.add_native_lambda_async
      "Listen.remove"
      ::state
      macro::false
      (
        fun args ctx::_ ::state cb::return =>
          switch args {
          | [Str name] =>
            switch (get_threadid state) {
            | Some threadid =>
              SqlHelper.remove_listen
                db
                !listens
                threadid
                name
                (
                  fun new_listens => {
                    listens := new_listens;
                    return (Ok (List []), state)
                  }
                )
            | None => return (id_not_loaded_ex, state)
            }
          | [_, _, _] =>
            return (
              AST.create_exception "Expected 2 strings and 1 function (name, pattern, handler) in call to Listen.add.",
              state
            )
          | _ =>
            return (
              BuiltinHelper.received_error
                expected::1 ::args name::"Listen.remove" ::state
            )
          }
      );
  state
};

let add_threadid_to_builtins threadid state =>
  Evaluate.Eval.define_native_symbol
    state "Thread.id" (Some "Id of the current thread.") (Str threadid);
