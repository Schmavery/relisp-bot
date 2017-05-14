type result 'a 'b =
  | Ok 'a
  | Error 'b;

type uuidT = string;

module StringMap = Map.Make String;

type astNodeT = {uuid: uuidT, value: valueT}
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
  state::evaluationStateT =>
  cb::((result astNodeT astNodeT, evaluationStateT) => unit) =>
  unit
and nativeFuncRecT = {func: nativeFuncT, is_macro: bool}
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
and evaluationStateT = {
  userTable: StringMap.t uuidT,
  symbolTable: StringMap.t uuidT,
  uuidToNodeMap: StringMap.t astNodeT
};

let gen_uuid () => {
  let s4 () => Printf.sprintf "%04x" (Random.int 65536);
  s4 () ^
  s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ s4 () ^ s4 ()
};

let stringmap_get key map =>
  try (Some (StringMap.find key map)) {
  | _ => None
  };

let stringmap_union m1 m2 =>
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

let create_node value => {uuid: gen_uuid (), value};

let create_exception text => Error (create_node (Str text));

let rec string_of_ast (ast: result astNodeT astNodeT) :string => {
  let string_of_func (f: funcT) => {
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
    let body = string_of_ast (Ok f.func);
    "[" ^ title ^ ": " ^ args ^ " => " ^ body ^ "]"
  };
  switch ast {
  | Ok value =>
    switch value.value {
    | Ident x => x
    | Str x => "'" ^ x ^ "'"
    | Num x => string_of_float x
    | Bool x => string_of_bool x
    | Ref _ => "[Ref]"
    | List x =>
      "(" ^
      (List.map (fun v => string_of_ast (Ok v)) x |> String.concat " ") ^ ")"
    | Func ({is_macro: false} as f) => string_of_func f
    | Func ({is_macro: true} as f) => string_of_func f
    | NativeFunc {is_macro: false} => "[Native Function]"
    | NativeFunc {is_macro: true} => "[Native Macro]"
    }
  | Error ex => "[Exception of " ^ string_of_ast (Ok ex) ^ "]"
  }
};

let string_of_stringmap table =>
  StringMap.fold (fun k v a => a ^ k ^ ":\t" ^ v ^ "\n") table "";

let string_of_state state =>
  "====state====\nuserTable:\n" ^
  string_of_stringmap state.userTable ^
  "-------------\nsymbolTable:\n" ^
  string_of_stringmap state.symbolTable ^ "\n=============";
