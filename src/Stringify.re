open Common;

let rec string_of_func (f: AST.funcT) state => {
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
  let body = string_of_ast (Ok f.func) state;
  "[" ^ kind ^ ": " ^ args ^ " => " ^ body ^ "]"
}
and string_of_exception (lst: list string, node: AST.astNodeT) state =>
  "[Exception of " ^
  string_of_ast (Ok node) state ^
  "]" ^ (
    if (List.length lst > 0) {
      "\nTrace: " ^ String.concat "\n       " (List.rev lst)
    } else {
      ""
    }
  )
and string_of_ast
    (ast: result AST.astNodeT AST.exceptionT)
    (state: AST.evalStateT)
    :string =>
  switch ast {
  | Ok value =>
    switch value {
    | Ident x => x
    | Str x => "\"" ^ x ^ "\""
    | Num x => Printf.sprintf "%g" x
    | Bool x => string_of_bool x
    | Ref refId =>
      "[Ref " ^
      (
        switch (StringMapHelper.get refId state.refMap) {
        | None => "?"
        | Some uuid =>
          switch (StringMapHelper.get uuid state.uuidToNodeMap) {
          | Some (Ref _) => "[Ref]"
          | Some x => string_of_ast (Ok x) state
          | None => "?"
          }
        }
      ) ^ "]"
    | List x =>
      "(" ^
      (List.map (fun v => string_of_ast (Ok v) state) x |> String.concat " ") ^ ")"
    | Map x =>
      "{" ^
      (
        List.map
          (
            fun (k, v) =>
              string_of_ast (Ok k) state ^ ": " ^ string_of_ast (Ok v) state
          )
          (ASTMap.bindings x) |>
        String.concat "\n "
      ) ^ "}"
    | Func ({is_macro: false} as f) => string_of_func f state
    | Func ({is_macro: true} as f) => string_of_func f state
    | NativeFunc {is_macro: false} => "[Native Function]"
    | NativeFunc {is_macro: true} => "[Native Macro]"
    }
  | Error ex => string_of_exception ex state
  };

let string_of_type (node: AST.astNodeT) :string =>
  switch node {
  | Ident _ => "ident"
  | Str _ => "string"
  | Num _ => "number"
  | Bool _ => "boolean"
  | Ref _ => "ref"
  | Map _ => "map"
  | List _ => "list"
  | Func _ => "function"
  | NativeFunc _ => "native function"
  };
