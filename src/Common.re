type result 'a 'b =
  | Ok 'a
  | Error 'b;

type uuidT = string;

type docsT = option string;

module StringMap = Map.Make String;

module StringMapHelper = {
  let get key map =>
    try (Some (StringMap.find key map)) {
    | _ => None
    };
  let union m1 m2 =>
    StringMap.merge
      (
        fun _key v1 v2 =>
          switch (v1, v2) {
          | (None, Some x)
          | (Some x, _) => Some x
          | _ => None
          }
      )
      m1
      m2;
  let to_string (table: StringMap.t string) =>
    StringMap.fold (fun k v a => a ^ k ^ ":\t" ^ v ^ "\n") table "";
};

module EvalState = {
  type t 'a = {
    userTable: StringMap.t (docsT, uuidT),
    symbolTable: StringMap.t (docsT, 'a),
    uuidToNodeMap: StringMap.t 'a,
    addedUuids: list uuidT
  };
  let empty = {
    userTable: StringMap.empty,
    uuidToNodeMap: StringMap.empty,
    symbolTable: StringMap.empty,
    addedUuids: []
  };
  let add_to_uuidmap (node: 'a) (uuid: uuidT) state :t 'a => {
    ...state,
    uuidToNodeMap: StringMap.add uuid node state.uuidToNodeMap,
    addedUuids: [uuid, ...state.addedUuids]
  };
  let add_to_symboltable ident_name node state => {
    ...state,
    symbolTable: StringMap.add ident_name node state.symbolTable
  };
  let add_to_usertable ident_name uuid state => {
    ...state,
    userTable: StringMap.add ident_name uuid state.userTable
  };
};

module type AST_Type = {
  type evalStateT = EvalState.t astNodeT
  and uuidOrNodeT =
    | Uuid uuidT
    | Node astNodeT
  and exceptionT = (list string, astNodeT)
  and funcT = {
    func: astNodeT,
    args: list string,
    vararg: option string,
    scope: StringMap.t (docsT, string),
    recur: uuidT,
    is_macro: bool
  }
  and nativeFuncT =
    list astNodeT =>
    ctx::ctxT =>
    state::evalStateT =>
    cb::((result astNodeT exceptionT, evalStateT) => unit) =>
    unit
  and nativeFuncRecT = {
    func: nativeFuncT,
    is_macro: bool
  }
  and ctxT = {
    argsTable: StringMap.t (docsT, uuidOrNodeT),
    depth: int
  }
  and astNodeT =
    | Ident string
    | Str string
    | Num float
    | Bool bool
    | Ref uuidT
    | List (list astNodeT)
    | Func funcT
    | NativeFunc nativeFuncRecT;
  let hash: astNodeT => uuidT;
  let create_exception: string => result 'a exceptionT;
  let to_string: result astNodeT exceptionT => string;
};

module AST: AST_Type = {
  type exceptionT = (list string, astNodeT)
  and uuidOrNodeT =
    | Uuid uuidT
    | Node astNodeT
  and funcT = {
    func: astNodeT,
    args: list string,
    vararg: option string,
    scope: StringMap.t (docsT, string),
    recur: uuidT,
    is_macro: bool
  }
  and nativeFuncT =
    list astNodeT =>
    ctx::ctxT =>
    state::evalStateT =>
    cb::((result astNodeT exceptionT, evalStateT) => unit) =>
    unit
  and nativeFuncRecT = {
    func: nativeFuncT,
    is_macro: bool
  }
  and ctxT = {
    argsTable: StringMap.t (docsT, uuidOrNodeT),
    depth: int
  }
  and astNodeT =
    | Ident string
    | Str string
    | Num float
    | Bool bool
    | Ref uuidT
    | List (list astNodeT)
    | Func funcT
    | NativeFunc nativeFuncRecT
  and evalStateT = EvalState.t astNodeT;
  let create_exception text => Error ([], Str text);
  let hash_hashstring s => {
    let hash = ref 0;
    for i in 0 to (String.length s - 1) {
      hash := !hash lsl 5 - !hash + Char.code s.[i]
    };
    string_of_int !hash
  };
  let rec hashstring_of_func (f: funcT) => {
    let kind =
      if f.is_macro {
        "macro"
      } else {
        "function"
      };
    let arg_list =
      switch f.vararg {
      | Some vararg_name => f.args @ ["... " ^ vararg_name]
      | None => f.args
      };
    let args = String.concat " " arg_list;
    let scope_list =
      StringMap.fold (fun k (_d, v) a => [k ^ v, ...a]) f.scope [];
    let scope = String.concat " " scope_list;
    /* Intentionally don't include the recur uuid in hash */
    "(" ^ kind ^ ":" ^ scope ^ ":" ^ args ^ ":" ^ to_hashstring f.func ^ ")"
  }
  and to_hashstring (ast: astNodeT) :string =>
    switch ast {
    | Ident x => x
    | Str x => "\"" ^ x ^ "\""
    | Num x => Printf.sprintf "%g" x
    | Bool x => string_of_bool x
    | Ref _ => "[Ref]"
    | List x =>
      "(" ^ (List.map (fun v => to_hashstring v) x |> String.concat " ") ^ ")"
    | Func f => hashstring_of_func f
    | NativeFunc _ => failwith "Cannot calculate hash of native func"
    };
  let hash node => hash_hashstring (to_hashstring node); /* todo: make this smaller */
  let rec string_of_func (f: funcT) => {
    let kind =
      if f.is_macro {
        "macro"
      } else {
        "function"
      };
    let arg_list =
      switch f.vararg {
      | Some vararg_name => f.args @ ["... " ^ vararg_name]
      | None => f.args
      };
    let args = String.concat ", " arg_list;
    let body = to_string (Ok f.func);
    "[" ^ kind ^ ": " ^ args ^ " => " ^ body ^ "]"
  }
  and string_of_exception (lst: list string, node: astNodeT) =>
    "[Exception of " ^
    to_string (Ok node) ^
    "]" ^ (
      if (List.length lst > 0) {
        "\nTrace: " ^ String.concat "\n       " (List.rev lst)
      } else {
        ""
      }
    )
  and to_string (ast: result astNodeT exceptionT) :string =>
    switch ast {
    | Ok value =>
      switch value {
      | Ident x => x
      | Str x => "\"" ^ x ^ "\""
      | Num x => Printf.sprintf "%g" x
      | Bool x => string_of_bool x
      | Ref _ => "[Ref]"
      | List x =>
        "(" ^
        (List.map (fun v => to_string (Ok v)) x |> String.concat " ") ^ ")"
      | Func ({is_macro: false} as f) => string_of_func f
      | Func ({is_macro: true} as f) => string_of_func f
      | NativeFunc {is_macro: false} => "[Native Function]"
      | NativeFunc {is_macro: true} => "[Native Macro]"
      }
    | Error ex => string_of_exception ex
    };
};
