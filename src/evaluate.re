open Common;

module type EvalT = {
  type t = evaluationStateT;
  let empty: t;
  let empty_node: astNodeT;
  let is_macro: astNodeT => bool;
  let create_initial_context: t => ctxT;
  let define_native_symbol: t => string => astNodeT => t;
  let define_user_symbol: t => string => astNodeT => t;
  let resolve_ident: string => ctxT => t => option astNodeT;
  let is_reserved_symbol: t => string => bool;
  let add_to_uuid_map: t => astNodeT => t;
  let eval: astNodeT => ctx::ctxT => state::t => (result astNodeT astNodeT, t);
  let eval_lambda:
    astNodeT =>
    args::list astNodeT =>
    func_name::string =>
    ctx::ctxT =>
    state::t =>
    (result astNodeT astNodeT, t);
};

let module Eval: EvalT = {
  type t = evaluationStateT;
  let trueNode = {uuid: gen_uuid (), value: Bool true};
  let falseNode = {uuid: gen_uuid (), value: Bool false};
  let empty_node = {uuid: gen_uuid (), value: List []};
  let max_stack = 512;
  let empty: t = {
    userTable: StringMap.empty,
    uuidToNodeMap: StringMap.empty,
    symbolTable: StringMap.empty
  };
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
  let is_reserved_symbol state ident_name => stringmap_get ident_name state.symbolTable == None;
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
  let resolve_ident (ident_name: string) (ctx: ctxT) (state: t) :option astNodeT => {
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
        | None => failwith ("Could not find node " ^ ident_name ^ " in uuidMap")
        }
      }
    }
  };

  /***/
  let rec create_lambda_arg_map
          (func_args: list string)
          (passed_args: list astNodeT)
          (map: StringMap.t astNodeT)
          :result (StringMap.t astNodeT) string =>
    switch (func_args, passed_args) {
    | ([], []) => Ok map
    | (["...", ident], rest) => Ok (StringMap.add ident {uuid: gen_uuid (), value: List rest} map)
    | ([ident, ...tl1], [value, ...tl2]) =>
      create_lambda_arg_map tl1 tl2 (StringMap.add ident value map)
    | ([], rest) =>
      let expected = StringMap.cardinal map;
      let received = expected + List.length rest;
      Error (
        "Expected " ^
        string_of_int expected ^ " arguments, received " ^ string_of_int received ^ " arguments."
      )
    | (rest, []) =>
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
        string_of_int expected ^ " arguments, received " ^ string_of_int received ^ " arguments."
      )
    };

  /** Mutually recursive main eval logic **/
  let rec eval
          ({value} as original_node: astNodeT)
          ctx::(ctx: ctxT)
          state::(state: t)
          :(result astNodeT astNodeT, t) =>
    switch value {
    | Ident ident =>
      switch (resolve_ident ident ctx state) {
      | Some resolved => (Ok resolved, state)
      | None => (create_exception ("Undeclared identifier [" ^ ident ^ "]."), state)
      }
    | List lst =>
      switch lst {
      | [] => (Ok original_node, state)
      | [first, ...args] =>
        let name =
          switch first {
          | {value: Ident i} => i
          | {value: List _} => "[Lambda function]"
          | _ => "Unknown"
          };
        let (evaled_first, state) = eval first ctx::ctx state::state;
        switch evaled_first {
        | Ok ({value: NativeFunc f} as func) when not f.is_macro =>
          switch (eval_lambda func args::args ctx::ctx func_name::name state::state) {
          | (Ok x, state) => eval x ctx::ctx state::state
          | (Error _, _) as e => e
          }
        | Ok ({value: NativeFunc _} as func)
        | Ok ({value: Func _} as func) =>
          eval_lambda func args::args ctx::ctx func_name::name state::state
        | Error e => (Error e, state)
        | Ok x => (
            create_exception (
              "Trying to call something that isn't a function. [" ^ string_of_ast (Ok x) ^ "]"
            ),
            state
          )
        }
      }
    | _ => (Ok original_node, state)
    }
  and eval_lambda
      ({uuid, value: func_value} as called_func: astNodeT)
      args::(args: list astNodeT)
      func_name::(func_name: string)
      ctx::(ctx: ctxT)
      state::state
      :(result astNodeT astNodeT, t) =>
    if (ctx.depth > max_stack) {
      (create_exception ("Stack overflow > " ^ string_of_int max_stack), state)
    } else {
      let maybe_args = eval_args called_func args ctx state;
      switch func_value {
      | NativeFunc native =>
        switch maybe_args {
        | (Ok args, state) => native.func args ctx::{...ctx, depth: ctx.depth + 1} state::state
        | (Error _, _) as e => e
        }
      | Func {func, args, scope, is_macro} =>
        switch maybe_args {
        | (Ok passed_args, state) =>
          let arg_to_node_map = create_lambda_arg_map args passed_args StringMap.empty;
          switch arg_to_node_map {
          | Error e => (create_exception e, state)
          | Ok map =>
            let argsUuidMap =
              StringMap.fold
                (fun key value acc => StringMap.add value.uuid value acc) map ctx.argsUuidMap;
            let arg_map = StringMap.map (fun value => value.uuid) map;
            let argsTable = stringmap_union arg_map scope;
            let ctx = {...ctx, argsUuidMap, argsTable};
            eval func ctx::ctx state::state
          }
        | (Error _, _) as e => e
        }
      | _ => assert false
      }
    }
  and eval_args
      (func: astNodeT)
      (args: list astNodeT)
      (ctx: ctxT)
      (state: t)
      :(result (list astNodeT) astNodeT, t) =>
    if (is_macro func) {
      (Ok args, state)
    } else {
      /* Eval args, accounting for exceptions */
      List.fold_right
        (
          fun e acc =>
            switch acc {
            | (Ok acc, state) =>
              switch (eval ctx::ctx e state::state) {
              | (Ok x, state) => (Ok [x, ...acc], state)
              | (Error _, _) as e => e
              }
            | x => x
            }
        )
        args
        (Ok [], state)
    };
};
