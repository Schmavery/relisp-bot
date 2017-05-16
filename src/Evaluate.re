open Common;

module type EvalT = {
  type t = evaluationStateT;
  let empty: t;
  let empty_node: astNodeT;
  let trueNode: astNodeT;
  let falseNode: astNodeT;
  let to_bool_node: bool => astNodeT;
  let is_macro: astNodeT => bool;
  let create_initial_context: t => ctxT;
  let define_native_symbol: t => string => astNodeT => t;
  let define_user_symbol: t => string => astNodeT => t;
  let resolve_ident: string => ctxT => t => option astNodeT;
  let is_reserved_symbol: t => string => bool;
  let add_to_uuid_map: t => astNodeT => t;
  let eval:
    astNodeT =>
    ctx::ctxT =>
    state::t =>
    cb::((result astNodeT astNodeT, t) => unit) =>
    unit;
  let eval_lambda:
    astNodeT =>
    args::list astNodeT =>
    func_name::string =>
    ctx::ctxT =>
    state::t =>
    cb::((result astNodeT astNodeT, t) => unit) =>
    unit;
};

module Eval: EvalT = {
  type t = evaluationStateT;
  let empty: t = {
    userTable: StringMap.empty,
    uuidToNodeMap: StringMap.empty,
    symbolTable: StringMap.empty
  };
  let empty_node = {uuid: gen_uuid (), value: List []};
  let trueNode = {uuid: gen_uuid (), value: Bool true};
  let falseNode = {uuid: gen_uuid (), value: Bool false};
  let to_bool_node b => if b {trueNode} else {falseNode};
  let max_stack = 512;
  let is_macro func :bool =>
    switch func {
    | {value: NativeFunc f} => f.is_macro
    | {value: Func f} => f.is_macro
    | _ => false
    };

  /***/
  let create_initial_context state => {
    argsUuidMap: StringMap.empty,
    argsTable: state.userTable,
    depth: 0
  };

  /***/
  let add_to_uuid_map (state: t) (node: astNodeT) :t => {
    ...state,
    uuidToNodeMap: StringMap.add node.uuid node state.uuidToNodeMap
  };

  /***/
  let is_reserved_symbol state ident_name =>
    stringmap_get ident_name state.symbolTable == None;
  let define_native_symbol (state: t) (ident_name: string) (node: astNodeT) :t => {
    ...state,
    symbolTable: StringMap.add ident_name node.uuid state.symbolTable,
    uuidToNodeMap: StringMap.add node.uuid node state.uuidToNodeMap
  };

  /***/
  let define_user_symbol (state: t) (ident_name: string) (node: astNodeT) :t => {
    ...state,
    userTable: StringMap.add ident_name node.uuid state.userTable,
    uuidToNodeMap: StringMap.add node.uuid node state.uuidToNodeMap
  };

  /***/
  let resolve_ident
      (ident_name: string)
      (ctx: ctxT)
      (state: t)
      :option astNodeT => {
    let uuid =
      switch (stringmap_get ident_name state.symbolTable) {
      | Some _ as uuid => uuid
      | None => stringmap_get ident_name ctx.argsTable
      };
    switch uuid {
    | None => None
    | Some x =>
      switch (stringmap_get x state.uuidToNodeMap) {
      | Some x => Some x
      | None =>
        switch (stringmap_get x ctx.argsUuidMap) {
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
      Ok (StringMap.add vararg {uuid: gen_uuid (), value: List rest} map)
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
          state::(state: t)
          ::cb
          :unit =>
    switch value {
    | Ident ident =>
      switch (resolve_ident ident ctx state) {
      | Some resolved => cb (Ok resolved, state)
      | None =>
        cb (create_exception ("Undeclared identifier [" ^ ident ^ "]."), state)
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
                        | (Error _, _) as e => cb e
                        }
                    }
                  )
              | Ok ({value: NativeFunc {is_macro: false}} as func)
              | Ok ({value: Func {is_macro: false}} as func) =>
                eval_lambda func ::args ::ctx func_name::name ::state ::cb
              | Error e => cb (Error e, state)
              | Ok x =>
                let node_str = string_of_ast (Ok x);
                cb (
                  create_exception (
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
        create_exception ("Stack overflow > " ^ string_of_int max_stack),
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
              | (Error _, _) as e => cb e
              }
            | Func {func, args, scope, vararg} =>
              switch maybe_args {
              | (Ok passed_args, state) =>
                let arg_to_node_map =
                  create_lambda_arg_map
                    vararg args passed_args StringMap.empty;
                switch arg_to_node_map {
                | Error e => cb (create_exception e, state)
                | Ok map =>
                  let argsUuidMap =
                    StringMap.fold
                      (fun _k value acc => StringMap.add value.uuid value acc)
                      map
                      ctx.argsUuidMap;
                  let arg_map = StringMap.map (fun value => value.uuid) map;
                  let argsTable = stringmap_union arg_map scope;
                  let ctx = {depth: ctx.depth + 1, argsUuidMap, argsTable};
                  eval func ::ctx ::state ::cb
                }
              | (Error _, _) as e => cb e
              }
            | _ => assert false
            }
        )
    }
  and eval_args
      (func: astNodeT)
      (args: list astNodeT)
      (ctx: ctxT)
      (state: t)
      (acc: list astNodeT)
      cb::(return: (result (list astNodeT) astNodeT, t) => unit)
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
