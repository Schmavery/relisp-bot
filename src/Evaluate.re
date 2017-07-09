open Common;

module type EvalT = {
  module AST: Common.AST_Type;
  type evalStateT = AST.evalStateT;
  type ctxT = AST.ctxT;
  type astNodeT = AST.astNodeT;
  let empty: evalStateT;
  let to_bool_node: bool => astNodeT;
  let is_macro: astNodeT => bool;
  let create_initial_context: evalStateT => ctxT;
  let define_native_symbol: evalStateT => string => astNodeT => evalStateT;
  let define_user_symbol: evalStateT => string => astNodeT => evalStateT;
  let resolve_ident: string => ctxT => evalStateT => option astNodeT;
  let is_reserved_symbol: evalStateT => string => bool;
  let add_to_uuid_map: evalStateT => astNodeT => evalStateT;
  let eval:
    astNodeT =>
    ctx::ctxT =>
    state::evalStateT =>
    cb::((result astNodeT AST.exceptionT, evalStateT) => unit) =>
    unit;
  let eval_lambda:
    astNodeT =>
    args::list astNodeT =>
    func_name::string =>
    ctx::ctxT =>
    state::evalStateT =>
    cb::((result astNodeT AST.exceptionT, evalStateT) => unit) =>
    unit;
};

module Eval (AST: AST_Type) :EvalT => {
  module AST = AST;
  module EvalState = AST.EvalState;
  module Constants = Common.Constants AST;
  type evalStateT = AST.evalStateT;
  type ctxT = AST.ctxT;
  type astNodeT = AST.astNodeT;
  let empty = EvalState.empty;
  let to_bool_node b => if b {Constants.true_node} else {Constants.false_node};
  let max_stack = 512;
  let is_macro (func: AST.astNodeT) :bool =>
    switch func {
    | {value: NativeFunc f} => f.is_macro
    | {value: Func f} => f.is_macro
    | _ => false
    };

  /***/
  let create_initial_context (state: evalStateT) => {
    AST.argsUuidMap: StringMap.empty,
    AST.argsTable: EvalState.usertable state,
    AST.depth: 0
  };

  /***/
  let add_to_uuid_map state node =>
    EvalState.add_to_uuidmap node.AST.uuid node state;

  /***/
  let is_reserved_symbol state ident_name =>
    StringMapHelper.get ident_name (EvalState.symboltable state) == None;

  /***/
  let define_native_symbol state (ident_name: string) (node: astNodeT) =>
    state |> EvalState.add_to_symboltable ident_name node.uuid |>
    EvalState.add_to_uuidmap node.uuid node;

  /***/
  let define_user_symbol state (ident_name: string) (node: astNodeT) =>
    state |> EvalState.add_to_usertable ident_name node.uuid |>
    EvalState.add_to_uuidmap node.uuid node;

  /***/
  let resolve_ident (ident_name: string) (ctx: ctxT) state :option astNodeT => {
    let uuid =
      switch (StringMapHelper.get ident_name (EvalState.symboltable state)) {
      | Some _ as uuid => uuid
      | None => StringMapHelper.get ident_name ctx.argsTable
      };
    switch uuid {
    | None => None
    | Some x =>
      switch (StringMapHelper.get x (EvalState.uuidmap state)) {
      | Some x => Some x
      | None =>
        switch (StringMapHelper.get x ctx.argsUuidMap) {
        | Some x => Some x
        | None =>
          failwith ("Could not find node " ^ ident_name ^ " in uuidMap")
        }
      }
    }
  };

  /***/
  let rec create_lambda_arg_map
          (vararg: option string)
          (func_args: list string)
          (passed_args: list astNodeT)
          (map: StringMap.t astNodeT)
          :result (StringMap.t astNodeT) string =>
    switch (func_args, passed_args, vararg) {
    | ([], [], None) => Ok map
    | ([], rest, Some vararg) =>
      Ok (StringMap.add vararg (AST.create_node (List rest)) map)
    | ([ident, ...tl1], [value, ...tl2], _) =>
      create_lambda_arg_map vararg tl1 tl2 (StringMap.add ident value map)
    | ([], rest, None) =>
      let expected = StringMap.cardinal map;
      let received = expected + List.length rest;
      Error (
        "Expected " ^
        string_of_int expected ^
        " arguments, received " ^ string_of_int received ^ " arguments."
      )
    | (rest, [], None)
    | (rest, [], Some _) =>
      let received = StringMap.cardinal map;
      let expected =
        (
          switch (List.rev rest) {
          | ["...", ...rest] => List.length rest - 1
          | _ => List.length rest
          }
        ) + received;
      Error (
        "Expected " ^
        string_of_int expected ^
        " arguments, received " ^ string_of_int received ^ " arguments."
      )
    };

  /** Mutually recursive main eval logic **/
  let rec eval
          ({value} as original_node: astNodeT)
          ctx::(ctx: ctxT)
          ::state
          ::cb
          :unit =>
    switch value {
    | Ident ident =>
      switch (resolve_ident ident ctx state) {
      | Some resolved => cb (Ok resolved, state)
      | None =>
        cb (
          AST.create_exception ("Undeclared identifier [" ^ ident ^ "]."),
          state
        )
      }
    | List lst =>
      switch lst {
      | [] => cb (Ok original_node, state)
      | [first, ...args] =>
        let name =
          switch first {
          | {value: Ident i} => i
          | {value: List _} => "[Lambda function]"
          | _ => "Unknown"
          };
        eval
          first
          ::ctx
          ::state
          cb::(
            fun (evaled_first, state) =>
              switch evaled_first {
              | Ok ({value: NativeFunc {is_macro: true}} as func)
              | Ok ({value: Func {is_macro: true}} as func) =>
                eval_lambda
                  func
                  ::args
                  ::ctx
                  func_name::name
                  ::state
                  cb::(
                    if (name == "quote" || name == "syntax-quote") {
                      cb
                    } else {
                      fun res =>
                        switch res {
                        | (Ok x, state) => eval x ::ctx ::state ::cb
                        | (Error (lst, ex), state) => cb (Error ([name, ...lst], ex), state)
                        }
                    }
                  )
              | Ok ({value: NativeFunc {is_macro: false}} as func)
              | Ok ({value: Func {is_macro: false}} as func) =>
                eval_lambda func ::args ::ctx func_name::name ::state ::cb
              /* | Error e => cb (Error e, state) */
              | Error (lst, ex) => cb (Error ([name, ...lst], ex), state)
              | Ok x =>
                let node_str = AST.to_string (Ok x);
                cb (
                  AST.create_exception (
                    "Trying to call something that isn't a function. [" ^
                    node_str ^ "]"
                  ),
                  state
                )
              }
          )
      }
    | _ => cb (Ok original_node, state)
    }
  and eval_lambda
      ({value: func_value} as called_func: astNodeT)
      args::(args: list astNodeT)
      func_name::(func_name: string)
      ctx::(ctx: ctxT)
      ::state
      ::cb =>
    if (ctx.depth > max_stack) {
      cb (
        AST.create_exception ("Stack overflow > " ^ string_of_int max_stack),
        state
      )
    } else {
      eval_args
        called_func
        args
        ctx
        state
        []
        cb::(
          fun maybe_args =>
            switch func_value {
            | NativeFunc native =>
              switch maybe_args {
              | (Ok args, state) =>
                native.func
                  args ctx::{...ctx, depth: ctx.depth + 1} ::state ::cb
              | (Error (trace, ex), s) => cb (Error ([func_name, ...trace], ex), s)
              }
            | Func {func, args, scope, vararg} =>
              switch maybe_args {
              | (Ok passed_args, state) =>
                let arg_to_node_map =
                  create_lambda_arg_map
                    vararg args passed_args StringMap.empty;
                switch arg_to_node_map {
                | Error e => cb (AST.create_exception e, state)
                | Ok map =>
                  let argsUuidMap =
                    StringMap.fold
                      (
                        fun _k value acc =>
                          StringMap.add value.AST.uuid value acc
                      )
                      map
                      ctx.argsUuidMap;
                  let arg_map =
                    StringMap.map (fun value => value.AST.uuid) map;
                  let argsTable = StringMapHelper.union arg_map scope;
                  let ctx = {
                    AST.depth: ctx.depth + 1,
                    AST.argsUuidMap: argsUuidMap,
                    AST.argsTable: argsTable
                  };
                  eval func ::ctx ::state ::cb
                }
              | (Error (trace, ex), s) => cb (Error ([func_name, ...trace], ex), s)
              }
            | _ => assert false
            }
        )
    }
  and eval_args
      (func: astNodeT)
      (args: list astNodeT)
      ctx
      state
      (acc: list astNodeT)
      cb::return
      :unit =>
    if (is_macro func) {
      return (Ok args, state)
    } else {
      switch args {
      | [] => return (Ok (List.rev acc), state)
      | [hd, ...tl] =>
        eval
          hd
          ::ctx
          ::state
          cb::(
            fun (res, state) =>
              switch res {
              | Ok v => eval_args func tl ctx state [v, ...acc] cb::return
              | Error e => return (Error e, state)
              }
          )
      }
    };
};
