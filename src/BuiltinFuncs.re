open Common;

module type EnvironmentT = {
  let load_lib: string => cb::(option string => unit) => unit;
};

module Builtins (Environment: EnvironmentT) (Eval: Evaluate.EvalT) => {
  module AST = Eval.AST;
  module Constants = Common.Constants AST;
  module Parser = Parse.Parser AST;
  let received_error ::expected ::args ::name ::state => (
    AST.create_exception (
      "Received " ^
      string_of_int (List.length args) ^
      " arguments, expected " ^
      string_of_int expected ^ " in call to '" ^ name ^ "'"
    ),
    state
  );
  let add_builtins state => {
    let add_native_lambda
        (name: string)
        macro::(is_macro: bool)
        func
        state::(state: Eval.evalStateT)
        :Eval.evalStateT => {
      let asyncf
          (args: list Eval.astNodeT)
          ctx::(ctx: Eval.ctxT)
          state::(state: Eval.evalStateT)
          cb::(
            cb:
              (
                result AST.astNodeT AST.exceptionT,
                Eval.evalStateT
              ) =>
              unit
          ) =>
        func args ::ctx ::state |> cb;
      let node = AST.create_node (NativeFunc {func: asyncf, is_macro});
      Eval.define_native_symbol state name node
    };
    let add_native_lambda_async
        (name: string)
        macro::(is_macro: bool)
        (func: AST.nativeFuncT)
        state::(state: Eval.evalStateT)
        :Eval.evalStateT => {
      let node = AST.create_node (NativeFunc {func, is_macro});
      Eval.define_native_symbol state name node
    };
    /* Define Builtins */
    let do_operation
        (op: float => float => float)
        (op_name: string)
        (state: Eval.evalStateT)
        :Eval.evalStateT =>
      add_native_lambda
        op_name
        ::state
        macro::false
        (
          fun args ::ctx ::state => (
            switch args {
            | [] =>
              AST.create_exception ("Called '" ^ op_name ^ "' with no args.")
            | [{value: Num _ as hd}, ...tl] =>
              let res =
                List.fold_left
                  (
                    fun acc v =>
                      switch (acc, v) {
                      | (Ok (AST.Num acc_num), {AST.value: Num x}) =>
                        Ok (AST.Num (op acc_num x))
                      | (Error _ as ex, _) => ex
                      | _ =>
                        AST.create_exception (
                          "Called '" ^
                          op_name ^ "' with something not a number."
                        )
                      }
                  )
                  (Ok hd)
                  tl;
              switch res {
              | Ok v => Ok (AST.create_node v)
              | Error e => Error e
              }
            | _ =>
              AST.create_exception (
                "Called '" ^ op_name ^ "' with something not a number."
              )
            },
            state
          )
        );
    let state = do_operation (+.) "+" state;
    let state = do_operation (-.) "-" state;
    let state = do_operation (/.) "/" state;
    let state = do_operation ( *. ) "*" state;
    let rec create_lambda_scope
            (node: Eval.astNodeT)
            (map: StringMap.t uuidT)
            (arg_names: list string)
            (ctx: Eval.ctxT)
            (state: Eval.evalStateT)
            :result (StringMap.t uuidT, Eval.evalStateT) string =>
      switch node {
      | {value: Ident i} =>
        switch (Eval.resolve_ident i ctx state) {
        | None => Ok (map, state)
        /* TODO: Intelligently parse body to statically check
           for undefined identifiers (idents could be captured by
           another lambda within the body of this lambda) */
        | Some x =>
          let new_state = Eval.add_to_uuid_map state x;
          Ok (StringMap.add i x.uuid map, new_state)
        }
      | {value: List lst} =>
        List.fold_left
          (
            fun acc v =>
              switch acc {
              | Ok (a, eval_state) =>
                create_lambda_scope v a arg_names ctx eval_state
              | x => x
              }
          )
          (Ok (map, state))
          lst
      | {value: Num _}
      | {value: Str _}
      | {value: Bool _} => Ok (map, state)
      | _ => Error "Found non-literal in lambda body."
      };
    let rec parse_lambda_args
            (args: list Eval.astNodeT)
            (acc: list string)
            :result (list string, option string) string =>
      switch args {
      | [] => Ok (List.rev acc, None)
      | [{value: Ident "..."}, {value: Ident vararg}] =>
        Ok (List.rev acc, Some vararg)
      | [{value: Ident "..."}, ..._] =>
        Error "Can only have ... before the last argument name."
      | [{value: Ident ident}, ...tl] => parse_lambda_args tl [ident, ...acc]
      | _ => Error "Argument defined as not identifier in function definition."
      };
    let parse_lambda_args args => parse_lambda_args args [];
    let create_lambda_func is_macro args ::ctx ::state => {
      let uuid = AST.gen_uuid ();
      let func_name = if is_macro {"macro"} else {"lambda"};
      switch args {
      | [{AST.value: List args}, body] =>
        let arg_names = parse_lambda_args args;
        switch arg_names {
        | Error e => (AST.create_exception e, state)
        | Ok (names, vararg) =>
          switch (
            create_lambda_scope
              body (StringMap.singleton "recur" uuid) names ctx state
          ) {
          | Ok (idents, new_state) =>
            let node = {
              AST.uuid: uuid,
              value:
                Func {func: body, args: names, scope: idents, is_macro, vararg}
            };
            /* add recur uuid */
            let new_state = Eval.add_to_uuid_map new_state node;
            (Ok node, new_state)
          | Error e => (AST.create_exception e, state)
          }
        }
      | [_, _] => (
          AST.create_exception (
            "Expected list as first argument in call to '" ^ func_name ^ "'"
          ),
          state
        )
      | args => received_error expected::2 ::args name::func_name ::state
      }
    };
    let state =
      add_native_lambda
        ::state "lambda" macro::true (create_lambda_func false);
    let state =
      add_native_lambda ::state "macro" macro::true (create_lambda_func true);
    let state =
      add_native_lambda
        ::state
        "throw"
        macro::false
        (
          fun args ::ctx ::state =>
            switch args {
            | [a] => (Error ([], a), state)
            | lst => received_error expected::2 args::lst name::"throw" ::state
            }
        );
    let state =
      add_native_lambda_async
        ::state
        "try"
        macro::true
        (
          fun args ::ctx ::state cb::return =>
            switch args {
            | [first, lambda] =>
              Eval.eval
                first
                ::ctx
                ::state
                cb::(
                  fun
                  | (Error (_trace, ex), state) =>
                    Eval.eval
                      lambda
                      ::ctx
                      ::state
                      cb::(
                        fun
                        | (Ok ({value: NativeFunc _} as func), state)
                        | (Ok ({value: Func _} as func), state) =>
                          Eval.eval_lambda
                            func
                            args::[ex]
                            ::ctx
                            func_name::"[Try]"
                            ::state
                            cb::return
                        | (_, state) =>
                          return (
                            AST.create_exception "Expected lambda as second argument in call to 'try'",
                            state
                          )
                      )
                  | x => return x
                )
            | lst =>
              return (received_error expected::2 args::lst name::"try" ::state)
            }
        );
    let state =
      add_native_lambda_async
        ::state
        "define"
        macro::true
        (
          fun args ::ctx ::state cb::return =>
            switch args {
            | [{value: Ident ident}, {value: Str _}, other]
            | [{value: Ident ident}, other] =>
              let docs =
                switch args {
                | [_, {value: Str docs}, _] => docs
                | _ => "No docs."
                };
              if (ctx.depth > 2) {
                return (
                  AST.create_exception "Can only define at the top level",
                  state
                )
              } else if (
                not (Eval.is_reserved_symbol state ident)
              ) {
                return (
                  AST.create_exception (
                    "Cannot define reserved keyword [" ^ ident ^ "]."
                  ),
                  state
                )
              } else {
                Eval.eval
                  other
                  ::ctx
                  ::state
                  cb::(
                    fun
                    | (Ok {value: NativeFunc _}, _) =>
                      return (
                        AST.create_exception "Cannot rename builtin function.",
                        state
                      )
                    | (Ok res, state) =>
                      return (
                        Ok Constants.empty_node,
                        Eval.define_user_symbol state ident res
                      )
                    | (Error _, _) as e => return e
                  )
              }
            | [_, _] =>
              return (
                AST.create_exception "Expected ident as first argument in call to 'define'",
                state
              )
            | lst =>
              return (
                received_error expected::2 args::lst name::"define" ::state
              )
            }
        );

    /** Quote-related logic */
    let state =
      add_native_lambda
        ::state
        "quote"
        macro::true
        (
          fun args ::ctx ::state =>
            switch args {
            | [el] => (Ok el, state)
            | lst => received_error expected::1 args::lst name::"quote" ::state
            }
        );
    let state =
      add_native_lambda_async
        ::state
        "unquote"
        macro::true
        (
          fun args ::ctx ::state cb::return =>
            switch args {
            | [el] => Eval.eval el ::ctx ::state cb::return
            | lst =>
              return (
                received_error expected::1 args::lst name::"unquote" ::state
              )
            }
        );
    let rec traverseH
            (lst: list Eval.astNodeT)
            (acc: list Eval.astNodeT)
            ::ctx
            ::state
            cb::(
              return:
                (
                  result (list AST.astNodeT) AST.exceptionT,
                  Eval.evalStateT
                ) =>
                unit
            )
            :unit =>
      switch lst {
      | [] => return (Ok acc, state)
      | [v, ...tl] =>
        switch v {
        | {value: List [{value: Ident "unquote-splice"}, unquote_arg]} =>
          Eval.eval
            unquote_arg
            ::ctx
            ::state
            cb::(
              fun (res, state) =>
                switch res {
                | Error _ as e => return (e, state)
                | Ok {value: List lst} =>
                  traverseH tl (acc @ lst) ::ctx ::state cb::return
                | Ok _ =>
                  return (
                    AST.create_exception "'unquote-splice' only applies to lists",
                    state
                  )
                }
            )
        | v =>
          traverse
            v
            ::ctx
            ::state
            cb::(
              fun (res, state) =>
                switch res {
                | Error _ as e => return (e, state)
                | Ok node =>
                  traverseH tl (acc @ [node]) ::ctx ::state cb::return
                }
            )
        }
      }
    and traverse
        (node: Eval.astNodeT)
        ::ctx
        state::(state: Eval.evalStateT)
        cb::(
          return:
            (
              result AST.astNodeT AST.exceptionT,
              Eval.evalStateT
            ) =>
            unit
        )
        :unit =>
      switch node {
      | {value: List [{value: Ident "unquote"}, next]} =>
        Eval.eval next ::ctx ::state cb::return
      | {value: List [{value: Ident "unquote"}, ...lst]} =>
        return (received_error expected::1 args::lst name::"unquote" ::state)
      | {value: List lst} =>
        traverseH
          lst
          []
          ::state
          ::ctx
          cb::(
            fun
            | (Ok a, state) => return (Ok (AST.create_node (List a)), state)
            | (Error a, state) => return (Error a, state)
          )
      | a => return (Ok a, state)
      };
    let state =
      add_native_lambda_async
        ::state
        "syntax-quote"
        macro::true
        (
          fun args ::ctx ::state cb::return =>
            switch args {
            | [e] => traverse e ::ctx ::state cb::return
            | lst =>
              return (
                received_error
                  expected::1 args::lst name::"syntax-quote" ::state
              )
            }
        );
    let state =
      add_native_lambda_async
        ::state
        "if"
        macro::true
        (
          fun args ::ctx ::state cb::return =>
            switch args {
            | [cond, consequent, alternate] =>
              Eval.eval
                cond
                ::state
                ::ctx
                cb::(
                  fun evaled_cond =>
                    switch (evaled_cond, consequent, alternate) {
                    | ((Error _, _) as e, _, _) => return e
                    | ((Ok {value: Bool true}, state), eval_me, _)
                    | ((Ok {value: Bool false}, state), _, eval_me) =>
                      return (Ok eval_me, state)
                    | ((Ok _, state), _, _) =>
                      return (
                        AST.create_exception "First argument to 'if' must evaluate to boolean.",
                        state
                      )
                    }
                )
            | lst =>
              return (received_error expected::3 args::lst name::"if" ::state)
            }
        );
    let rec eval_list
            (lst: list Eval.astNodeT)
            (state: Eval.evalStateT)
            (ctx: Eval.ctxT)
            cb::return =>
      switch lst {
      | [] => return (Ok state)
      | [hd, ...tl] =>
        Eval.eval
          hd
          ::state
          ::ctx
          cb::(
            fun
            | (Ok _, state) =>
              eval_list tl state (Eval.create_initial_context state) cb::return
            | (Error e, _) => return (Error e)
          )
      };
    /* TODO: handle loading in a loop */
    let state =
      add_native_lambda_async
        ::state
        "load"
        macro::false
        (
          fun args ::ctx ::state cb::return =>
            switch args {
            | [{value: Str lib_name}] =>
              Environment.load_lib
                lib_name
                cb::(
                  fun text =>
                    switch text {
                    | None =>
                      return (
                        AST.create_exception (
                          "Could not load lib " ^ lib_name
                        ),
                        state
                      )
                    | Some text =>
                      switch (Parser.parse_multi (String.trim text)) {
                      | Ok lst =>
                        eval_list
                          lst
                          state
                          ctx
                          cb::(
                            fun
                            | Ok state =>
                              return (Ok Constants.empty_node, state)
                            | Error e => return (Error e, state)
                          )
                      | Error e => return (Error e, state)
                      }
                    }
                )
            | [_] =>
              return (
                AST.create_exception "Expected string as first argument in call to 'load'",
                state
              )
            | lst =>
              return (
                received_error expected::1 args::lst name::"load" ::state
              )
            }
        );
    let rec equal_helper
            {AST.value: v1, AST.uuid: uuid1}
            {AST.value: v2, AST.uuid: uuid2} =>
      uuid1 == uuid2 ||
      v1 == v2 || (
        switch (v1, v2) {
        | (Ident s1, Ident s2)
        | (Str s1, Str s2) => s1 == s2
        | (Num n1, Num n2) => n1 == n2
        | (Bool b1, Bool b2) => b1 == b2
        | (Ref u1, Ref u2) => u1 == u2
        | (List l1, List l2) =>
          try (
            List.fold_left2
              (fun acc e1 e2 => acc || equal_helper e1 e2) true l1 l2
          ) {
          | Invalid_argument _ => false
          }
        | (
            Func {func: f1, args: a1, vararg: v1, scope: s1, is_macro: m1},
            Func {func: f2, args: a2, vararg: v2, scope: s2, is_macro: m2}
          ) =>
          m1 == m2 && a1 == a2 && v1 == v2 && s1 == s2 && equal_helper f1 f2
        | (NativeFunc _, NativeFunc _) =>
          /*This is only true if they have the same uuid, checked above*/ false
        | _ => false
        }
      );
    let state =
      add_native_lambda
        ::state
        "="
        macro::false
        (
          fun args ::ctx ::state =>
            switch args {
            | [e1, e2] => (Ok (Eval.to_bool_node (equal_helper e1 e2)), state)
            | lst =>
              received_error expected::2 args::lst name::"equal?" ::state
            }
        );

    /** List manipulation functions */
    let state =
      add_native_lambda
        ::state
        "car"
        macro::false
        (
          fun args ::ctx ::state =>
            switch args {
            | [{value: List [first, ..._]}] => (Ok first, state)
            | [{value: List []}] => (
                AST.create_exception "Called 'car' on empty list",
                state
              )
            | [e] => (
                AST.create_exception (
                  "Expected list in call to 'car', got [" ^
                  AST.to_string (Ok e) ^ "] instead."
                ),
                state
              )
            | lst => received_error expected::1 args::lst name::"car" ::state
            }
        );
    let state =
      add_native_lambda
        ::state
        "cdr"
        macro::false
        (
          fun args ::ctx ::state =>
            switch args {
            | [{value: List [_, ...rest]}] => (
                Ok (AST.create_node (List rest)),
                state
              )
            | [{value: List []}] => (
                AST.create_exception "Called 'cdr' on empty list",
                state
              )
            | [_] => (
                AST.create_exception "Expected list in call to 'cdr'",
                state
              )
            | lst => received_error expected::1 args::lst name::"cdr" ::state
            }
        );
    let state =
      add_native_lambda
        ::state
        "cons"
        macro::false
        (
          fun args ::ctx ::state =>
            switch args {
            | [el, {value: List lst}] => (
                Ok (AST.create_node (List [el, ...lst])),
                state
              )
            | [_, _] => (
                AST.create_exception "Expected list as second arg in call to 'cons'",
                state
              )
            | lst => received_error expected::2 args::lst name::"cons" ::state
            }
        );

    /** Debug functions */
    let state =
      add_native_lambda
        ::state
        "DEBUG/print-scope"
        macro::false
        (
          fun args ::ctx ::state =>
            switch args {
            | [{value: Func {scope}}] =>
              print_endline (StringMapHelper.to_string scope);
              (Ok Constants.empty_node, state)
            | lst =>
              received_error
                expected::1 args::lst name::"DEBUG/print-scope" ::state
            }
        );
    let state =
      add_native_lambda
        ::state
        "DEBUG/print-state"
        macro::false
        (
          fun args ::ctx ::state =>
            switch args {
            | [] =>
              print_endline (AST.EvalState.to_string state);
              (Ok Constants.empty_node, state)
            | lst =>
              received_error
                expected::0 args::lst name::"DEBUG/print-state" ::state
            }
        );
    let state =
      add_native_lambda_async
        ::state
        "DEBUG/expand-macro"
        macro::false
        (
          fun args ::ctx state::initial_state ::cb =>
            switch args {
            | [{value: List [first, ...args]}] =>
              Eval.eval
                first
                ::ctx
                state::initial_state
                cb::(
                  fun (evaled_first, state) =>
                    switch evaled_first {
                    | Ok ({value: NativeFunc {is_macro: true}} as func)
                    | Ok ({value: Func {is_macro: true}} as func) =>
                      Eval.eval_lambda
                        func ::args ::ctx func_name::"expand" ::state ::cb
                    | Ok _ as e =>
                      cb (
                        AST.create_exception (AST.to_string e),
                        initial_state
                      )
                    | Error _ as e => cb (e, initial_state)
                    }
                )
            | [_] =>
              cb (
                AST.create_exception "Expected list in call to 'DEBUG/expand-macro",
                initial_state
              )
            | lst =>
              cb (
                received_error
                  expected::0
                  args::lst
                  name::"DEBUG/expand-macro"
                  state::initial_state
              )
            }
        );
    state
  };
};
