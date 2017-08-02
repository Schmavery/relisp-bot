open Common;

module Builtins (Environment: BuiltinHelper.EnvironmentT) => {
  module Eval = Evaluate.Eval;
  module Parser = Parse.Parser;
  open BuiltinHelper;
  let add_builtins state => {
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
            (map: StringMap.t (docsT, uuidT))
            (arg_names: list string)
            (ctx: AST.ctxT)
            (state: AST.evalStateT)
            :result (StringMap.t (docsT, uuidT), AST.evalStateT) string =>
      switch node {
      | Ident i =>
        switch (Eval.resolve_ident_with_docs i ctx state) {
        | None
        /* Don't remember native functions (they're from symboltable)*/
        | Some (_, NativeFunc _) => Ok (map, state)
        /* TODO: Intelligently parse body to statically check
           for undefined identifiers (idents could be captured by
           another lambda within the body of this lambda) */
        | Some (docs, node) =>
          let hash = Hash.hash node;
          let new_state = Eval.add_to_uuid_map state hash node;
          Ok (StringMap.add i (docs, hash) map, new_state)
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
            let hash = Hash.hash node_dummy;
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
      | args =>
        received_error
          expected::["args:list", "body:list"] ::args name::func_name ::state
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
            | lst =>
              received_error_num expected::1 args::lst name::"throw" ::state
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
              return (
                received_error_num expected::2 args::lst name::"try" ::state
              )
            }
        );
    let state =
      add_native_lambda_async
        ::state
        "define"
        docs::(
          "Takes an identifier and a value to be set to that identifier. " ^ "Optionally, a string can be passed in after the identifier to be used as docs."
        )
        macro::true
        (
          fun args ::ctx ::state cb::return =>
            switch args {
            | [Ident ident, Str _, other]
            | [Ident ident, other] =>
              let docs =
                switch args {
                | [_, Str docs, _] => Some docs
                | _ => None
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
                        Eval.define_user_symbol state ident docs res
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
                received_error_num expected::2 args::lst name::"define" ::state
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
            | lst =>
              received_error_num expected::1 args::lst name::"quote" ::state
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
                received_error_num
                  expected::1 args::lst name::"unquote" ::state
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
        return (
          received_error_num expected::1 args::lst name::"unquote" ::state
        )
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
                received_error_num
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
              return (
                received_error_num expected::3 args::lst name::"if" ::state
              )
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
            | lst =>
              return (
                received_error
                  expected::["string"] args::lst name::"load" ::state
              )
            }
        );
    let rec equal_helper (v1: AST.astNodeT) (v2: AST.astNodeT) =>
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
              received_error_num expected::2 args::lst name::"equal?" ::state
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
            | lst =>
              received_error expected::["list"] args::lst name::"car" ::state
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
            | lst =>
              received_error expected::["list"] args::lst name::"cdr" ::state
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
            | lst =>
              received_error
                expected::["any", "list"] args::lst name::"cons" ::state
            }
        );
    let state =
      add_native_lambda
        ::state
        "docs"
        macro::true
        (
          fun args ::ctx ::state =>
            switch args {
            | [Ident i] =>
              switch (Evaluate.Eval.resolve_ident_with_docs i ctx state) {
              | None =>
                switch (EvalState.find_closest_idents state i) {
                | [] => (
                    AST.create_exception (
                      "Undeclared identifier [" ^ i ^ "]."
                    ),
                    state
                  )
                | suggestions =>
                  let formatted_suggestions =
                    String.concat
                      ", " (List.map (fun s => "[" ^ s ^ "]") suggestions);
                  (
                    AST.create_exception (
                      "Undeclared identifier [" ^
                      i ^ "]. Did you mean " ^ formatted_suggestions ^ "?"
                    ),
                    state
                  )
                }
              | Some (None, _) => (Ok (Str "No docs"), state)
              | Some (Some docs, _) => (Ok (Str docs), state)
              }
            | lst =>
              received_error expected::["ident"] args::lst name::"docs" ::state
            }
        );

    /** Refs */
    let state =
      add_native_lambda
        ::state
        "ref"
        macro::false
        (
          fun args ctx::_ ::state =>
            switch args {
            | [a] =>
              let hash = Hash.hash a;
              let refId = Common.gen_uuid ();
              let state = Common.EvalState.add_to_uuidmap a hash state;
              let state = Common.EvalState.update_ref refId hash state;
              (Ok (Ref refId), state)
            | lst =>
              received_error_num expected::1 args::lst name::"ref" ::state
            }
        );
    let state =
      add_native_lambda
        ::state
        "set!"
        macro::false
        (
          fun args ctx::_ ::state =>
            switch args {
            | [Ref refId, a] =>
              let hash = Hash.hash a;
              let state = Common.EvalState.add_to_uuidmap a hash state;
              let state = Common.EvalState.update_ref refId hash state;
              (Ok (List []), state)
            | lst =>
              received_error
                expected::["ref", "any"] args::lst name::"set!" ::state
            }
        );
    let state =
      add_native_lambda
        ::state
        "deref"
        macro::false
        (
          fun args ctx::_ ::state =>
            switch args {
            | [Ref refId] => (
                switch (StringMapHelper.get refId state.refMap) {
                | None => AST.create_exception "Cannot find refID"
                | Some uuid =>
                  switch (StringMapHelper.get uuid state.uuidToNodeMap) {
                  | Some x => Ok x
                  | None => AST.create_exception "Cannot find uuid"
                  }
                },
                state
              )
            | lst =>
              received_error expected::["ref"] args::lst name::"deref" ::state
            }
        );
    let state =
      Evaluate.Eval.define_native_symbol
        state "Map.empty" (Some "Empty map.") (Map ASTMap.empty);
    let state =
      add_native_lambda
        ::state
        "Map.get"
        macro::false
        (
          fun args ctx::_ ::state =>
            switch args {
            | [Map map, key] =>
              switch (ASTMap.find key map) {
              | v => (Ok v, state)
              | exception _ => (
                  AST.create_exception (
                    "Could not find key: " ^
                    Stringify.string_of_ast (Ok key) state
                  ),
                  state
                )
              }
            | lst =>
              received_error
                expected::["map", "key"] args::lst name::"Map.get" ::state
            }
        );
    let state =
      add_native_lambda
        ::state
        "Map.add"
        macro::false
        (
          fun args ctx::_ ::state =>
            switch args {
            | [Map map, key, v] => (Ok (Map (ASTMap.add key v map)), state)
            | lst =>
              received_error
                expected::["map", "key", "value"]
                args::lst
                name::"Map.add"
                ::state
            }
        );
    let state =
      add_native_lambda
        ::state
        "Map.len"
        macro::false
        (
          fun args ctx::_ ::state =>
            switch args {
            | [Map map] => (
                Ok (Num (float_of_int (ASTMap.cardinal map))),
                state
              )
            | lst =>
              received_error
                expected::["map"] args::lst name::"Map.len" ::state
            }
        );
    let state =
      add_native_lambda
        ::state
        "Map.entries"
        macro::false
        (
          fun args ctx::_ ::state =>
            switch args {
            | [Map map] => (
                Ok (
                  List (
                    ASTMap.fold
                      (
                        fun k v (acc: list AST.astNodeT) => [
                          List [k, v],
                          ...acc
                        ]
                      )
                      map
                      []
                  )
                ),
                state
              )
            | lst =>
              received_error
                expected::["map"] args::lst name::"Map.entries" ::state
            }
        );
    let state =
      add_native_lambda
        ::state
        "Map.remove"
        macro::false
        (
          fun args ctx::_ ::state =>
            switch args {
            | [Map map, key] => (Ok (Map (ASTMap.remove key map)), state)
            | lst =>
              received_error
                expected::["map", "key"] args::lst name::"Map.remove" ::state
            }
        );
    let state =
      add_native_lambda
        ::state
        "String.join"
        macro::false
        (
          fun args ctx::_ ::state =>
            switch args {
            | [Str sep, List lst] =>
              let maybeStrs =
                List.fold_right
                  (
                    fun (v: AST.astNodeT) acc =>
                      switch (acc, v) {
                      | (Some acc, Str s) => Some [s, ...acc]
                      | _ => None
                      }
                  )
                  lst
                  (Some []);
              switch maybeStrs {
              | Some strs => (Ok (Str (String.concat sep strs)), state)
              | None =>
                received_error
                  expected::["sep:string", "list of strings"]
                  ::args
                  name::"String.join"
                  ::state
              }
            | _ =>
              received_error
                expected::["sep:string", "list of strings"]
                ::args
                name::"String.join"
                ::state
            }
        );

    /** Debug functions */
    /* let state = */
    /*   add_native_lambda */
    /*     ::state */
    /*     "DEBUG/print-scope" */
    /*     macro::false */
    /*     ( */
    /*       fun args ctx::_ ::state => */
    /*         switch args { */
    /*         | [Func {scope}] => */
    /*           print_endline (StringMapHelper.to_string scope); */
    /*           (Ok (List []), state) */
    /*         | lst => */
    /*           received_error */
    /*             expected::1 args::lst name::"DEBUG/print-scope" ::state */
    /*         } */
    /*     ); */
    let state =
      add_native_lambda
        ::state
        "to-string"
        macro::false
        (
          fun args ctx::_ ::state =>
            switch args {
            | [a] => (Ok (Str (Stringify.string_of_ast (Ok a) state)), state)
            | _ =>
              received_error_num expected::1 ::args name::"to-string" ::state
            }
        );
    let state =
      add_native_lambda
        ::state
        "Debug.print-state"
        macro::false
        (
          fun args ctx::_ ::state =>
            switch args {
            | [] =>
              print_endline (Common.EvalState.to_string state);
              (Ok (List []), state)
            | lst =>
              received_error_num
                expected::0 args::lst name::"Debug.print-state" ::state
            }
        );
    let state =
      add_native_lambda_async
        ::state
        "Debug.expand-macro"
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
                        AST.create_exception (Stringify.string_of_ast e state),
                        initial_state
                      )
                    | Error _ as e => cb (e, initial_state)
                    }
                )
            | lst =>
              cb (
                received_error
                  expected::["list"]
                  args::lst
                  name::"Debug.expand-macro"
                  state::initial_state
              )
            }
        );
    state
  };
};
