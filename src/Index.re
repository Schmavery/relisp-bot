open Result;

type uuidT = string;

let module StringMap = Map.Make String;

type astNodeT = {uuid: uuidT, value: valueT}
and funcT = {func: astNodeT, args: list string, scope: StringMap.t string, is_macro: bool}
and nativeFuncT = {func: list astNodeT => ctx::ctxT => result astNodeT astNodeT, is_macro: bool}
and ctxT = {argsUuidMap: StringMap.t astNodeT}
and valueT =
  | Ident string
  | Str string
  | Num float
  | Bool bool
  | Ref uuidT
  | List (list astNodeT)
  | Func funcT
  | NativeFunc nativeFuncT;

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

/*  Constants */
let max_stack = 512;

let localStack: ref (list (StringMap.t string)) = ref [StringMap.empty];

let uuidToNodeMap: ref (StringMap.t astNodeT) = ref StringMap.empty;

let reservedUuids: ref (StringMap.t bool) = ref StringMap.empty;

let symbolTable: ref (StringMap.t astNodeT) = ref StringMap.empty;

let gen_uuid () => {
  let s4 () => Printf.sprintf "%04x" (Random.int 65536);
  s4 () ^ s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ "-" ^ s4 () ^ s4 () ^ s4 ()
};

let trueNode = {uuid: gen_uuid (), value: Bool true};

let falseNode = {uuid: gen_uuid (), value: Bool false};

let stringmap_get key map =>
  try {
    StringMap.find key map;
    false
  } {
  | Not_found => true
  };

let stringmap_get key map =>
  try (Some (StringMap.find key map)) {
  | Not_found => None
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

let rec create_lambda_arg_map
        (func_args: list string)
        (passed_args: list astNodeT)
        (map: StringMap.t astNodeT)
        :result (StringMap.t astNodeT) string =>
  switch (func_args, passed_args) {
  | ([], []) => Ok map
  | ([ident, "..."], rest) => Ok (StringMap.add ident {uuid: gen_uuid (), value: List rest} map)
  | ([ident, ...tl1], [value, ...tl2]) =>
    create_lambda_arg_map tl1 tl2 (StringMap.add ident value map)
  | ([], rest) =>
    let expected = StringMap.cardinal map;
    let received = expected + List.length rest;
    Error (
      "Expected " ^
      string_of_int expected ^ "arguments, received " ^ string_of_int received ^ "arguments."
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
      string_of_int expected ^ "arguments, received " ^ string_of_int received ^ "arguments."
    )
  };

/* Evaluation logic */
let get_local (ident_name: string) (ctx: ctxT) :option astNodeT => {
  let uuid = stringmap_get ident_name (List.hd !localStack);
  switch uuid {
  | Some x =>
    switch (stringmap_get x !uuidToNodeMap) {
    | Some x => Some x
    | None =>
      switch (stringmap_get x ctx.argsUuidMap) {
      | Some x => Some x
      | None => failwith ("Could not find node " ^ ident_name ^ " in uuidMap")
      }
    }
  | None => None
  }
};

let get_native (ident_name: string) :option astNodeT => {
  let uuid = stringmap_get ident_name !symbolTable;
  switch uuid {
  | Some x => Some x
  | None => None
  }
};

let resolve_ident (ident_name: string) (ctx: ctxT) :option astNodeT => {
  let native = get_native ident_name;
  switch native {
  | None => get_local ident_name ctx
  | x => x
  }
};

let add_native_lambda name is_macro func => {
  let node = create_node (NativeFunc {func, is_macro});
  symbolTable := StringMap.add name node !symbolTable
};

let is_macro func :bool =>
  switch func {
  | {value: NativeFunc f} => f.is_macro
  | {value: Func f} => f.is_macro
  | _ => false
  };

let rec eval ({value} as original_node: astNodeT) ctx::(ctx: ctxT) :result astNodeT astNodeT =>
  switch value {
  | Ident ident =>
    switch (get_native ident) {
    | Some native => Ok native
    | None =>
      switch (get_local ident ctx) {
      | Some local => Ok local
      | None => create_exception ("Undeclared identifier [" ^ ident ^ "].")
      }
    }
  | List lst =>
    switch lst {
    | [] => Ok original_node
    | [first, ...args] =>
      let name =
        switch first {
        | {value: Ident i} => i
        | {value: List _} => "[Lambda function]"
        | _ => "Unknown"
        };
      let evaled_first = eval first ctx::ctx;
      switch evaled_first {
      | Ok ({value: NativeFunc f} as func) when not f.is_macro =>
        switch (eval_lambda func args::args ctx::ctx func_name::name) {
        | Ok x => eval x ctx::ctx
        | Error x => Error x
        }
      | Ok ({value: NativeFunc _} as func)
      | Ok ({value: Func _} as func) => eval_lambda func args::args ctx::ctx func_name::name
      | Error e => Error e
      | Ok x =>
        create_exception (
          "Trying to call something that isn't a function. [" ^ string_of_ast (Ok x) ^ "]"
        )
      }
    }
  | _ => Ok original_node
  }
and eval_lambda
    ({uuid, value: func_value} as called_func: astNodeT)
    args::(args: list astNodeT)
    ctx::(ctx: ctxT)
    func_name::(func_name: string)
    :result astNodeT astNodeT =>
  if (List.length !localStack > max_stack) {
    create_exception ("Stack overflow > " ^ string_of_int max_stack)
  } else {
    let maybe_args = eval_args called_func args ctx;
    switch func_value {
    | NativeFunc native =>
      localStack := [StringMap.empty, ...!localStack];
      switch maybe_args {
      | Ok args =>
        let result = native.func args ctx::ctx;
        localStack := List.tl !localStack;
        result
      | Error e => Error e
      }
    | Func {func, args, scope, is_macro} =>
      switch maybe_args {
      | Ok passed_args =>
        let arg_to_node_map = create_lambda_arg_map args passed_args StringMap.empty;
        switch arg_to_node_map {
        | Error e => create_exception e
        | Ok map =>
          let new_ctx = {
            argsUuidMap:
              StringMap.fold
                (fun key value acc => StringMap.add value.uuid value acc) map ctx.argsUuidMap
          };
          let arg_map = StringMap.map (fun value => value.uuid) map;
          let stack_frame = stringmap_union arg_map scope;
          localStack := [stack_frame, ...!localStack];
          let result = eval func ctx::new_ctx;
          localStack := List.tl !localStack;
          result
        }
      | Error e => Error e
      }
    | _ => assert false
    }
  }
and eval_args (func: astNodeT) (args: list astNodeT) (ctx: ctxT) :result (list astNodeT) astNodeT =>
  if (is_macro func) {
    Ok args
  } else {
    /* Eval args, accounting for exceptions */
    List.fold_left
      (
        fun maybe_acc e =>
          switch maybe_acc {
          | Ok acc =>
            let res = eval ctx::ctx e;
            switch res {
            | Ok x => Ok [x, ...acc]
            | Error e => Error e
            }
          | x => x
          }
      )
      (Ok [])
      args
  };

/* Define Builtins */
let do_operation (op: float => float => float) (op_name: string) :unit =>
  add_native_lambda
    op_name
    false
    (
      fun args ctx::ctx =>
        switch args {
        | [] => create_exception ("Called '" ^ op_name ^ "' with no args.")
        | [{value: Num _ as hd}, ...tl] =>
          let res =
            List.fold_left
              (
                fun acc v =>
                  switch (acc, v) {
                  | (Ok (Num acc_num), {value: Num x}) => Ok (Num (op acc_num x))
                  | (Error _ as ex, _) => ex
                  | _ => create_exception ("Called '" ^ op_name ^ "' with something not a number.")
                  }
              )
              (Ok hd)
              tl;
          switch res {
          | Ok v => Ok (create_node v)
          | Error e => Error e
          }
        | _ => create_exception ("Called '" ^ op_name ^ "' with something not a number.")
        }
    );

do_operation (+.) "+";

do_operation (-.) "-";

do_operation (/.) "/";

do_operation ( *. ) "*";

let rec create_lambda_scope
        (node: astNodeT)
        (map: StringMap.t uuidT)
        (arg_names: list string)
        (ctx: ctxT)
        :result (StringMap.t uuidT) string =>
  switch node {
  | {uuid, value: Ident i} =>
    switch (resolve_ident i ctx) {
    | None => Ok map
    /* TODO: Intelligently parse body to statically check
       for undefined identifiers (idents could be captured by
       another lambda within the body of this lambda) */
    | Some x =>
      uuidToNodeMap := StringMap.add x.uuid x !uuidToNodeMap;
      Ok (StringMap.add i x.uuid map)
    }
  | {uuid, value: List lst} =>
    List.fold_left
      (
        fun acc v =>
          switch acc {
          | Ok a => create_lambda_scope v a arg_names ctx
          | x => x
          }
      )
      (Ok map)
      lst
  | {value: Num _}
  | {value: Str _}
  | {value: Bool _} => Ok map
  | _ => Error "Found non-literal in lambda body."
  };

add_native_lambda
  "lambda"
  true
  (
    fun args ctx::ctx => {
      let uuid = gen_uuid ();
      switch args {
      | [{value: List args}, {value: List _} as body] =>
        let arg_names =
          List.fold_left
            (
              fun acc v =>
                switch acc {
                | Ok a =>
                  switch v {
                  | {value: Ident ident} => Ok [ident, ...a]
                  | _ => Error "Argument defined as not identifier in 'lambda'"
                  }
                | x => x
                }
            )
            (Ok [])
            args;
        switch arg_names {
        | Error e => create_exception e
        | Ok names =>
          switch (create_lambda_scope body (StringMap.singleton "recur" uuid) names ctx) {
          | Ok idents =>
            let node = {
              uuid,
              value: Func {func: body, args: names, scope: idents, is_macro: false}
            };
            /* add recur uuid */
            uuidToNodeMap := StringMap.add uuid node !uuidToNodeMap;
            Ok node
          | Error e => create_exception e
          }
        }
      | [_, _] => create_exception "Expected 2 lists in call to 'lambda'"
      | _ =>
        let received = string_of_int (List.length args);
        create_exception ("Received " ^ received ^ " arguments, expected 2 in call to 'lambda'")
      }
    }
  );

add_native_lambda
  "throw"
  false
  (
    fun args ctx::ctx =>
      switch args {
      | [a] => Error a
      | lst =>
        let received = string_of_int (List.length lst);
        create_exception ("Received " ^ received ^ " arguments, expected 2 in call to 'throw'")
      }
  );

add_native_lambda
  "try"
  true
  (
    fun args ctx::ctx =>
      switch args {
      | [first, lambda] =>
        switch (eval first ctx::ctx) {
        | Error ex =>
          switch (eval lambda ctx::ctx) {
          | Ok ({value: NativeFunc _} as func)
          | Ok ({value: Func _} as func) =>
            eval_lambda func args::[ex] ctx::ctx func_name::"[Catch]"
          | _ => create_exception "Expected lambda as second argument in call to 'catch'"
          }
        | x => x
        }
      | lst =>
        let received = string_of_int (List.length lst);
        create_exception ("Received " ^ received ^ " arguments, expected 2 in call to 'catch'")
      }
  );

/* Parsing logic */
let module Stream = {
  type t = list char;
  let empty () => [];
  let peek (stream: t) :option char =>
    switch stream {
    | [] => None
    | [c, ..._] => Some c
    };
  let pop (stream: t) :t =>
    switch stream {
    | [] => raise (Failure "Empty")
    | [_, ...lst] => lst
    };
  let create (s: string) :t => {
    let rec explode (i: int) (acc: list char) :t =>
      if (i < 0) {
        acc
      } else {
        explode (i - 1) [s.[i], ...acc]
      };
    explode (String.length s - 1) []
  };
};

type parseResult =
  | ParseOk (Stream.t, astNodeT)
  | ParseFail string
  | UnexpectedEnd;

let parse s => {
  let is_num c => Char.code c >= Char.code '0' && Char.code c <= Char.code '9';

  /** atom parsers **/
  let rec parse_string (stream: Stream.t) (acc: string) :parseResult =>
    switch (Stream.peek stream) {
    | Some '\\' =>
      let pop_str = Stream.pop stream;
      switch (Stream.peek pop_str) {
      | Some 'n' => parse_string (Stream.pop pop_str) (acc ^ String.make 1 '\n')
      | Some 't' => parse_string (Stream.pop pop_str) (acc ^ String.make 1 '\t')
      | Some '"' => parse_string (Stream.pop pop_str) (acc ^ String.make 1 '"')
      | Some '\'' => parse_string (Stream.pop pop_str) (acc ^ String.make 1 '\'')
      | Some '\\' => parse_string (Stream.pop pop_str) (acc ^ String.make 1 '\\')
      | Some c => ParseFail ("Invalid escape sequence \\" ^ String.make 1 c ^ ".")
      | None => ParseFail "Unterminated string."
      }
    | Some '"' => ParseOk (Stream.pop stream, create_node (Str acc))
    | Some c => parse_string (Stream.pop stream) (acc ^ String.make 1 c)
    | None => ParseFail "Unterminated string."
    };
  let rec parse_ident (stream: Stream.t) (acc: string) :parseResult =>
    switch (Stream.peek stream) {
    | Some ' '
    | Some '\t'
    | Some '\n'
    | Some '('
    | Some ')'
    | Some '"'
    | None => ParseOk (stream, create_node (Ident acc))
    | Some c => parse_ident (Stream.pop stream) (acc ^ String.make 1 c)
    };
  let rec parse_num (stream: Stream.t) (acc: string) :parseResult =>
    switch (Stream.peek stream) {
    | Some c when is_num c => parse_num (Stream.pop stream) (acc ^ String.make 1 c)
    | _ =>
      let num =
        try (Some (float_of_string acc)) {
        | Failure "float_of_string" => None
        };
      switch num {
      | Some f => ParseOk (stream, create_node (Num f))
      | None => ParseFail ("Could not parse number [" ^ acc ^ "].")
      }
    };

  /** Mututally recursive parse functions **/
  let rec parse (stream: Stream.t) :parseResult =>
    switch (Stream.peek stream) {
    | Some '"' => parse_string (Stream.pop stream) ""
    | Some '(' => parse_list (Stream.pop stream) []
    | Some ' '
    | Some '\n' => parse (Stream.pop stream)
    | Some c when is_num c => parse_num stream ""
    | Some c => parse_ident stream ""
    | None => UnexpectedEnd
    }
  and parse_list (stream: Stream.t) (acc: list astNodeT) :parseResult =>
    switch (Stream.peek stream) {
    | Some ')' => ParseOk (Stream.pop stream, create_node (List (List.rev acc)))
    | Some c =>
      switch (parse stream) {
      | ParseOk (res_stream, res) => parse_list res_stream [res, ...acc]
      | UnexpectedEnd => ParseFail "Unterminated list."
      | ParseFail _ as e => e
      }
    | None => ParseFail "Unterminated list."
    };
  let stream = Stream.create s;
  switch (parse stream) {
  | ParseOk (_, node) => Ok node
  | ParseFail error => create_exception error
  | UnexpectedEnd => create_exception "Unexpected end of input."
  }
};

let rec main () => {
  let in_str = input_line stdin;
  if (in_str != "exit") {
    string_of_ast (
      switch (parse in_str) {
      | Ok e => eval e ctx::{argsUuidMap: StringMap.empty}
      | Error e => Error e
      }
    ) |> print_endline;
    main ()
  }
};

Random.self_init ();

main ();
