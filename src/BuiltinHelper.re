open Common;

module type EnvironmentT = {
  let load_lib: string => cb::(option string => unit) => unit;
};

let received_error ::expected ::args ::name ::state => (
  AST.create_exception (
    "Received " ^
    string_of_int (List.length args) ^
    " arguments, expected " ^
    string_of_int expected ^ " in call to '" ^ name ^ "'"
  ),
  state
);

let add_native_lambda
    (name: string)
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
  Evaluate.Eval.define_native_symbol state name node
};

let add_native_lambda_async
    (name: string)
    macro::(is_macro: bool)
    (func: AST.nativeFuncT)
    state::(state: AST.evalStateT)
    :AST.evalStateT => {
  let node = AST.NativeFunc {func, is_macro};
  Evaluate.Eval.define_native_symbol state name node
};
