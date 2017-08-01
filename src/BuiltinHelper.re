open Common;

module type EnvironmentT = {
  let load_lib: string => cb::(option string => unit) => unit;
};

let received_error_num ::expected ::args ::name ::state => (
  AST.create_exception (
    "Received " ^
    string_of_int (List.length args) ^
    " arguments, expected " ^
    string_of_int expected ^ " in call to '" ^ name ^ "'"
  ),
  state
);

let received_error
    expected::(expected: list string)
    ::args
    ::name
    ::state
    :(result AST.astNodeT AST.exceptionT, AST.evalStateT) => {
  /* let expected_num = List.length expected; */
  /* let received_num = List.length args; */
  /* if (expected_num == received_num) { */
  let expected_str = String.concat ", " expected;
  let received_str =
    String.concat ", " (List.map Stringify.string_of_type args);
  (
    AST.create_exception (
      "Received [" ^
      received_str ^
      "], expected [" ^ expected_str ^ "] in call to '" ^ name ^ "'"
    ),
    state
  )
  /* } else { */
  /*   ( */
  /*     AST.create_exception ( */
  /*       "Received " ^ */
  /*       string_of_int received_num ^ */
  /*       " arguments, expected " ^ */
  /*       string_of_int expected_num ^ " in call to '" ^ name ^ "'" */
  /*     ), */
  /*     state */
  /*   ) */
  /* } */
};

let add_native_lambda
    (name: string)
    ::docs=?
    macro::(is_macro: bool)
    func
    state::(state: AST.evalStateT)
    :AST.evalStateT => {
  let asyncf
      (args: list AST.astNodeT)
      ctx::(ctx: AST.ctxT)
      state::(state: AST.evalStateT)
      cb::(cb: (result AST.astNodeT AST.exceptionT, AST.evalStateT) => unit) =>
    func args ::ctx ::state |> cb;
  let node = AST.NativeFunc {func: asyncf, is_macro};
  Evaluate.Eval.define_native_symbol state name docs node
};

let add_native_lambda_async
    (name: string)
    ::docs=?
    macro::(is_macro: bool)
    (func: AST.nativeFuncT)
    state::(state: AST.evalStateT)
    :AST.evalStateT => {
  let node = AST.NativeFunc {func, is_macro};
  Evaluate.Eval.define_native_symbol state name docs node
};
