type result 'a 'b =
  | Ok 'a
  | Error 'b;

type uuidT = string;

module StringMap = Map.Make String;

module StringMapHelper = {
  let get key map =>
    try (Some (StringMap.find key map)) {
    | _ => None
    };
  let union m1 m2 =>
    StringMap.merge
      (
        fun key v1 v2 =>
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
    userTable: StringMap.t uuidT,
    symbolTable: StringMap.t 'a,
    uuidToNodeMap: StringMap.t 'a,
    addedUuids: list uuidT
  };
  let empty = {
    userTable: StringMap.empty,
    uuidToNodeMap: StringMap.empty,
    symbolTable: StringMap.empty,
    addedUuids: []
  };
  let to_string state =>
    "====state====\nuserTable:\n" ^
    StringMapHelper.to_string state.userTable ^
    "-------------\nsymbolTable:\n" ^
    StringMap.fold (fun k _v a => a ^ k ^ "\n") state.symbolTable "";
  let add_to_uuidmap uuid node state => {
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
  and exceptionT = (list string, astNodeT)
  and astNodeT = {
    uuid: uuidT,
    value: valueT
  }
  and funcT = {
    func: astNodeT,
    args: list string,
    vararg: option string,
    scope: StringMap.t string,
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
    argsUuidMap: StringMap.t astNodeT,
    argsTable: StringMap.t uuidT,
    depth: int
  }
  and valueT =
    | Ident string
    | Str string
    | Num float
    | Bool bool
    | Ref uuidT
    | List (list astNodeT)
    | Func funcT
    | NativeFunc nativeFuncRecT;
  let create_node: valueT => astNodeT;
  let gen_uuid: unit => uuidT;
  let create_exception: string => result 'a exceptionT;
  let to_string: result astNodeT exceptionT => string;
};

module AST: AST_Type = {
  type astNodeT = {
    uuid: uuidT,
    value: valueT
  }
  and exceptionT = (list string, astNodeT)
  and funcT = {
    func: astNodeT,
    args: list string,
    vararg: option string,
    scope: StringMap.t string,
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
    argsUuidMap: StringMap.t astNodeT,
    argsTable: StringMap.t uuidT,
    depth: int
  }
  and valueT =
    | Ident string
    | Str string
    | Num float
    | Bool bool
    | Ref uuidT
    | List (list astNodeT)
    | Func funcT
    | NativeFunc nativeFuncRecT
  and evalStateT = EvalState.t astNodeT;
  let gen_uuid () => {
    let s4 () => Printf.sprintf "%04x" (Random.int 65536);
    s4 () ^
    s4 () ^
    "-" ^ s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ s4 () ^ s4 ()
  };
  let create_node value => {uuid: gen_uuid (), value};
  let create_exception text => Error ([], create_node (Str text));
  let rec string_of_func (f: funcT) => {
    let title =
      if f.is_macro {
        "Macro"
      } else {
        "Function"
      };
    let arg_list =
      switch f.vararg {
      | Some vararg_name => f.args @ ["... " ^ vararg_name]
      | None => f.args
      };
    let args = String.concat ", " arg_list;
    let body = to_string (Ok f.func);
    "[" ^ title ^ ": " ^ args ^ " => " ^ body ^ "]"
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
      switch value.value {
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
