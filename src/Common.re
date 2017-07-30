type result 'a 'b =
  | Ok 'a
  | Error 'b;

type uuidT = string;

type refIdT = string;

type docsT = option string;

let gen_uuid () => {
  let s4 () => Printf.sprintf "%04x" (Random.int 65536);
  s4 () ^
  s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ s4 () ^ s4 ()
};

module StringMap = Map.Make String;

module StringMapHelper = {
  let get key map =>
    try (Some (StringMap.find key map)) {
    | _ => None
    };
  let get_default key map default =>
    try (StringMap.find key map) {
    | _ => default
    };
  let update_default key map ::default update => {
    let curr_v =
      try (StringMap.find key map) {
      | _ => default
      };
    StringMap.add key (update curr_v) map
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
  type actionT =
    | AddUuid uuidT
    | UpdateRef (refIdT, uuidT);
  type t 'a = {
    userTable: StringMap.t (docsT, uuidT),
    symbolTable: StringMap.t (docsT, 'a),
    uuidToNodeMap: StringMap.t 'a,
    refMap: StringMap.t uuidT,
    recentActions: list actionT
  };
  let empty = {
    userTable: StringMap.empty,
    uuidToNodeMap: StringMap.empty,
    symbolTable: StringMap.empty,
    refMap: StringMap.empty,
    recentActions: []
  };
  let update_ref ref_id new_val state :t 'a => {
    ...state,
    refMap: StringMap.add ref_id new_val state.refMap,
    recentActions: [UpdateRef (ref_id, new_val), ...state.recentActions]
  };
  let add_to_uuidmap (node: 'a) (uuid: uuidT) state :t 'a => {
    ...state,
    uuidToNodeMap: StringMap.add uuid node state.uuidToNodeMap,
    recentActions: [AddUuid uuid, ...state.recentActions]
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
  type astMapT 'a;
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
    | Ref refIdT
    | List (list astNodeT)
    | Map (astMapT astNodeT)
    | Func funcT
    | NativeFunc nativeFuncRecT;
  let create_exception: string => result 'a exceptionT;
};

module ASTFunc (ASTMap: Map.S) => {
  type astMapT 'a = ASTMap.t 'a;
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
    | Ref refIdT
    | List (list astNodeT)
    | Map (astMapT astNodeT)
    | Func funcT
    | NativeFunc nativeFuncRecT
  and evalStateT = EvalState.t astNodeT;
  let create_exception text => Error ([], Str text);
};

module rec AST: AST_Type with type astMapT 'a = ASTMap.t 'a = ASTFunc ASTMap
and ASTMap: Map.S with type key = AST.astNodeT =
  Map.Make {
    type t = AST.astNodeT;
    let compare = compare;
  };
