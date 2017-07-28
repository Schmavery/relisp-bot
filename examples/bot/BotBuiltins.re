open Common;

let add_builtins state api => {
  let state =
    BuiltinHelper.add_native_lambda_async
      "THREAD/names"
      ::state
      macro::false
      (
        fun args ctx::_ ::state cb::return =>
          switch args {
          | [] =>
            switch (
              StringMapHelper.get "THREAD/id" state.EvalState.symbolTable
            ) {
            | Some (_docs, Str threadid) =>
              ChatApi.getThreadInfo
                api
                id::threadid
                (
                  fun _ threadInfo =>
                    switch (Js.Null.to_opt threadInfo) {
                    | None =>
                      return (
                        Common.AST.create_exception "THREAD/names failed because of getThreadInfo.",
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
                                Common.AST.create_exception "THREAD/names failed because of getUserInfo.",
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
            | Some _ =>
              return (
                Common.AST.create_exception "THREAD/id not string!",
                state
              )
            | None =>
              return (
                Common.AST.create_exception "THREAD/id not loaded",
                state
              )
            };
            ()
          | _ =>
            return (
              BuiltinHelper.received_error
                expected::0 ::args name::"THREAD/names" ::state
            )
          }
      );
  let state =
    BuiltinHelper.add_native_lambda_async
      "THREAD/ids"
      ::state
      macro::false
      (
        fun args ctx::_ ::state cb::return =>
          switch args {
          | [] =>
            switch (
              StringMapHelper.get "THREAD/id" state.EvalState.symbolTable
            ) {
            | Some (_docs, Str threadid) =>
              ChatApi.getThreadInfo
                api
                id::threadid
                (
                  fun _ threadInfo =>
                    switch (Js.Null.to_opt threadInfo) {
                    | None =>
                      return (
                        Common.AST.create_exception "THREAD/ids failed because of getThreadInfo.",
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
            | Some _ =>
              return (
                Common.AST.create_exception "THREAD/id not string!",
                state
              )
            | None =>
              return (
                Common.AST.create_exception "THREAD/id not loaded",
                state
              )
            };
            ()
          | _ =>
            return (
              BuiltinHelper.received_error
                expected::0 ::args name::"THREAD/ids" ::state
            )
          }
      );
  let state =
    BuiltinHelper.add_native_lambda_async
      "THREAD/message"
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
                      Common.AST.create_exception "THREAD/message failed.",
                      state
                    )
                  | Some _ => return (Ok (List []), state)
                  }
              )
          | [_, _] =>
            return (
              AST.create_exception "Expected 2 strings (id, msg) in call to THREAD/message.",
              state
            )
          | _ =>
            return (
              BuiltinHelper.received_error
                expected::2 ::args name::"THREAD/message" ::state
            )
          }
      );
  state
};

let add_threadid_to_builtins threadid state =>
  Evaluate.Eval.define_native_symbol
    state "THREAD/id" (Some "Id of the current thread.") (Str threadid);
