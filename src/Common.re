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

module StringCmp = {
  let minimum a b c => min a (min b c);
  /* Calculates Levenshtein distance: number of changes to turn
   * string s into string t. Lower score = more similar strings. */
  let distance s t => {
    let m = String.length s
    and n =
      String.length t; /* for all i and j, d.(i).(j) will hold the Levenshtein distance between
     the first i characters of s and the first j characters of t */
    let d = Array.make_matrix (m + 1) (n + 1) 0;
    for i in 0 to m {
      d.(i).(0) =
        i /* the distance of any first string to an empty second string */
    };
    for j in 0 to n {
      d.(0).(j) =
        j /* the distance of any second string to an empty first string */
    };
    for j in 1 to n {
      for i in 1 to m {
        if (s.[i - 1] == t.[j - 1]) {
          d.(i).(j) = d.(i - 1).(j - 1)
        } else {
          /* no operation required */
          d.(i).(j) =
            minimum
              (d.(i - 1).(j) + 1) /* a deletion */
              (d.(i).(j - 1) + 1) /* an insertion */
              (d.(i - 1).(j - 1) + 1) /* a substitution */
        }
      }
    };
    d.(m).(n)
  };
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
  let to_string state =>
    "UserTable:" ^
    StringMap.fold
      (fun k (_d, v) a => a ^ k ^ ":\t" ^ v ^ "\n") state.userTable "";
  let search_map map ident input :option (int, list string) =>
    StringMap.fold
      (
        fun new_ident _uuid acc => {
          let dist = StringCmp.distance ident new_ident;
          switch acc {
          | None => Some (dist, [new_ident])
          | Some (score, suggestions) as curr =>
            if (score > dist) {
              Some (dist, [new_ident])
            } else if (
              score == dist
            ) {
              Some (dist, [new_ident, ...suggestions])
            } else {
              curr
            }
          }
        }
      )
      map
      input;
  let find_closest_idents state ident :list string => {
    let results =
      search_map state.userTable ident None |>
      search_map state.symbolTable ident;
    switch results {
    | Some (score, suggestions) when score < 10 => suggestions
    | None
    | Some _ => []
    }
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
