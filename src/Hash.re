open Common;

/* open AST; */
let hash_hashstring s => {
  let hash = ref 0;
  for i in 0 to (String.length s - 1) {
    hash := !hash lsl 5 - !hash + Char.code s.[i]
  };
  string_of_int !hash
};

let rec hashstring_of_func (f: AST.funcT) => {
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
and to_hashstring (ast: AST.astNodeT) :string =>
  switch ast {
  | Ident x => x
  | Str x => "\"" ^ x ^ "\""
  | Num x => Printf.sprintf "%g" x
  | Bool x => string_of_bool x
  | Ref refId => "[Ref " ^ refId ^ "]"
  | List x =>
    "(" ^ (List.map (fun v => to_hashstring v) x |> String.concat " ") ^ ")"
  | Map x =>
    "{" ^
    (
      List.map
        (fun (k, v) => to_hashstring k ^ ":" ^ to_hashstring v)
        (ASTMap.bindings x) |>
      String.concat "\n"
    ) ^ "}"
  | Func f => hashstring_of_func f
  | NativeFunc _ => failwith "Cannot calculate hash of native func"
  };

let hash node => hash_hashstring (to_hashstring node); /* todo: make this smaller */
