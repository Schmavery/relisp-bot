type result 'a 'b =
  | Ok 'a
  | Error 'b;

type uuidT = string;

let module StringMap = Map.Make String;

type astNodeT = {uuid: uuidT, value: valueT}
and funcT = {func: astNodeT, args: list string, scope: StringMap.t string, is_macro: bool}
and nativeFuncT =
  list astNodeT =>
  ctx::ctxT =>
  state::evaluationStateT =>
  (result astNodeT astNodeT, evaluationStateT)
and nativeFuncRecT = {func: nativeFuncT, is_macro: bool}
and ctxT = {argsUuidMap: StringMap.t astNodeT}
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
  localStack: list (StringMap.t string),
  uuidToNodeMap: StringMap.t astNodeT,
  symbolTable: StringMap.t astNodeT
};

let gen_uuid () => {
  let s4 () => Printf.sprintf "%04x" (Random.int 65536);
  s4 () ^ s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ s4 () ^ s4 ()
};

let stringmap_get key map => try (Some (StringMap.find key map)){
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

let rec string_of_ast (ast: result astNodeT astNodeT) :string =>
  switch ast {
  | Ok value =>
    switch value.value {
    | Ident x => "[" ^ x ^ "]"
    | Str x => "'" ^ x ^ "'"
    | Num x => string_of_float x
    | Bool x => string_of_bool x
    | Ref _ => "[Ref]"
    | List x => "(" ^ (List.map (fun v => string_of_ast (Ok v)) x |> String.concat " ") ^ ")"
    | Func _ => "[Function]"
    | NativeFunc _ => "[Native Function]"
    }
  | Error ex => "[Exception of " ^ string_of_ast (Ok ex) ^ "]"
  };
