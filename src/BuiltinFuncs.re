open Common;

module type EnvironmentT = {
  let load_lib: string => cb::(option string => unit) => unit;
};

module Builtins (Environment: EnvironmentT) => {
  module Eval = Evaluate.Eval;
  module AST = Common.AST;
  module Constants = Constants;
  module Parser = Parse.Parser;
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
        state::(state: AST.evalStateT)
        :AST.evalStateT => {
      let asyncf
          (args: list AST.astNodeT)
          ctx::(ctx: AST.ctxT)
          state::(state: AST.evalStateT)
          cb::(
            cb: (result AST.astNodeT AST.exceptionT, AST.evalStateT) => unit
          ) =>
        func args ::ctx ::state |> cb;
      let node = AST.NativeFunc {func: asyncf, is_macro};
      Eval.define_native_symbol state name node
    };
    let add_native_lambda_async
        (name: string)
        macro::(is_macro: bool)
        (func: AST.nativeFuncT)
        state::(state: AST.evalStateT)
        :AST.evalStateT => {
      let node = AST.NativeFunc {func, is_macro};
      Eval.define_native_symbol state name node
    };
    /* Define Builtins */
    let do_operation
        (op: float => float => float)
        (op_name: string)
        (state: AST.evalStateT)
        :AST.evalStateT =>
      add_native_lambda
        op_name
        ::state
        macro::false
        (
          fun args ctx::_ ::state => (
            switch args {
            | [] =>
              AST.create_exception ("Called '" ^ op_name ^ "' with no args.")
            | [Num _ as hd, ...tl] =>
              let res =
                List.fold_left
                  (
                    fun acc v =>
                      switch (acc, v) {
                      | (Ok (AST.Num acc_num), AST.Num x) =>
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
              | Ok v => Ok v
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
            (node: AST.astNodeT)
            (map: StringMap.t uuidT)
            (arg_names: list string)
            (ctx: AST.ctxT)
            (state: AST.evalStateT)
            :result (StringMap.t uuidT, AST.evalStateT) string =>
      switch node {
      | Ident i =>
        switch (Eval.resolve_ident i ctx state) {
        | None
        /* Don't remember native functions (they're from symboltable)*/
        | Some (NativeFunc _) => Ok (map, state)
        /* TODO: Intelligently parse body to statically check
           for undefined identifiers (idents could be captured by
           another lambda within the body of this lambda) */
        | Some node =>
          let hash = AST.hash node;
          let new_state = Eval.add_to_uuid_map state hash node;
          Ok (StringMap.add i hash map, new_state)
        }
      | List lst =>
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
      | Num _
      | Str _
      | Bool _ => Ok (map, state)
      | _ => Error "Found non-literal in lambda body."
      };
    let rec parse_lambda_args
            (args: list AST.astNodeT)
            (acc: list string)
            :result (list string, option string) string =>
      switch args {
      | [] => Ok (List.rev acc, None)
      | [Ident "...", Ident vararg] => Ok (List.rev acc, Some vararg)
      | [Ident "...", ..._] =>
        Error "Can only have ... before the last argument name."
      | [Ident ident, ...tl] => parse_lambda_args tl [ident, ...acc]
      | _ => Error "Argument defined as not identifier in function definition."
      };
    let parse_lambda_args args => parse_lambda_args args [];
    let create_lambda_func is_macro (args: list AST.astNodeT) ::ctx ::state => {
      let func_name = if is_macro {"macro"} else {"lambda"};
      switch args {
      | [List args, body] =>
        let arg_names = parse_lambda_args args;
        switch arg_names {
        | Error e => (AST.create_exception e, state)
        | Ok (names, vararg) =>
          switch (create_lambda_scope body StringMap.empty names ctx state) {
          | Ok (idents, new_state) =>
            /* Here, we need to make a dummy node with all the correct values
             * except for the recur uuid, then calculate the hash of it.  The hash
             * ignores the recur field, so it will be the same as the function
             * node with the correct hash inserted for recur, which is created
             * afterward. */
            let node_dummy =
              AST.Func {
                func: body,
                args: names,
                scope: idents,
                is_macro,
                vararg,
                recur: "dummy"
              };
            let hash = AST.hash node_dummy;
            let node =
              AST.Func {
                func: body,
                args: names,
                scope: idents,
                is_macro,
                vararg,
                recur: hash
              };
            /* add recur uuid */
            let new_state = Eval.add_to_uuid_map new_state hash node;
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
          fun args ctx::_ ::state =>
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
                        | (Ok (NativeFunc _ as func), state)
                        | (Ok (Func _ as func), state) =>
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
            | [Ident ident, Str _, other]
            | [Ident ident, other] =>
              let _docs = /* TODO: docs */
                switch args {
                | [_, Str docs, _] => docs
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
                    | (Ok (NativeFunc _), _) =>
                      return (
                        AST.create_exception "Cannot rename builtin function.",
                        state
                      )
                    | (Ok res, state) =>
                      return (
                        Ok (List []),
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
          fun args ctx::_ ::state =>
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
            (lst: list AST.astNodeT)
            (acc: list AST.astNodeT)
            ::ctx
            ::state
            cb::(
              return:
                (result (list AST.astNodeT) AST.exceptionT, AST.evalStateT) =>
                unit
            )
            :unit =>
      switch lst {
      | [] => return (Ok acc, state)
      | [v, ...tl] =>
        switch v {
        | List [Ident "unquote-splice", unquote_arg] =>
          Eval.eval
            unquote_arg
            ::ctx
            ::state
            cb::(
              fun (res, state) =>
                switch res {
                | Error _ as e => return (e, state)
                | Ok (List lst) =>
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
        (node: AST.astNodeT)
        ::ctx
        state::(state: AST.evalStateT)
        cb::(
          return: (result AST.astNodeT AST.exceptionT, AST.evalStateT) => unit
        )
        :unit =>
      switch node {
      | List [Ident "unquote", next] => Eval.eval next ::ctx ::state cb::return
      | List [Ident "unquote", ...lst] =>
        return (received_error expected::1 args::lst name::"unquote" ::state)
      | List lst =>
        traverseH
          lst
          []
          ::state
          ::ctx
          cb::(
            fun
            | (Ok a, state) => return (Ok (List a), state)
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
                    | ((Ok (Bool true), state), eval_me, _)
                    | ((Ok (Bool false), state), _, eval_me) =>
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
            (lst: list AST.astNodeT)
            (state: AST.evalStateT)
            (ctx: AST.ctxT)
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
            | [Str lib_name] =>
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
                            | Ok state => return (Ok (List []), state)
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
    let rec equal_helper (v1 : AST.astNodeT)  (v2 : AST.astNodeT) =>
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
          fun args ctx::_ ::state =>
            switch args {
            | [e1, e2] => (Ok (Bool (equal_helper e1 e2)), state)
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
          fun args ctx::_ ::state =>
            switch args {
            | [List [first, ..._]] => (Ok first, state)
            | [List []] => (
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
          fun args ctx::_ ::state =>
            switch args {
            | [List [_, ...rest]] => (Ok (List rest), state)
            | [List []] => (
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
          fun args ctx::_ ::state =>
            switch args {
            | [el, List lst] => (Ok (List [el, ...lst]), state)
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
          fun args ctx::_ ::state =>
            switch args {
            | [Func {scope}] =>
              print_endline (StringMapHelper.to_string scope);
              (Ok (List []), state)
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
          fun args ctx::_ ::state =>
            switch args {
            | [] =>
              print_endline (Common.EvalState.to_string state);
              (Ok (List []), state)
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
            | [List [first, ...args]] =>
              Eval.eval
                first
                ::ctx
                state::initial_state
                cb::(
                  fun (evaled_first, state) =>
                    switch evaled_first {
                    | Ok (NativeFunc {is_macro: true} as func)
                    | Ok (Func {is_macro: true} as func) =>
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
