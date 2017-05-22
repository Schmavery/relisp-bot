module Environment: BuiltinFuncs.EnvironmentT = {
  type errorT;
  external readFile :
    string =>
    _ [@bs.as "utf8"] =>
    (errorT => Js.Undefined.t string => unit) =>
    unit =
    "" [@@bs.module "fs"];
  let load_lib filename ::cb =>
    readFile
      ("stdlib/" ^ filename ^ ".lib")
      (fun _ str => cb (Js.Undefined.to_opt str));
};

module Readline = {
  type pipeT;
  type interfaceDefT;
  type interfaceT;
  external createInterfaceDef : input::pipeT => output::pipeT => interfaceDefT =
    "" [@@bs.obj];
  external createInterface : interfaceDefT => interfaceT =
    "createInterface" [@@bs.module "readline"];
  external question : interfaceT => string => (string => unit) => unit =
    "question" [@@bs.send];
  external stdin : pipeT = "process.stdin" [@@bs.val];
  external stdout : pipeT = "process.stdout" [@@bs.val];
};

let rlDef =
  Readline.createInterfaceDef input::Readline.stdin output::Readline.stdout;

let rl = Readline.createInterface rlDef;

Random.self_init ();

module AST = Common.AST Common.BasicEvalState;

module Eval = Evaluate.Eval AST;

module Parser = Parse.Parser Eval.AST;

module Builtins = BuiltinFuncs.Builtins Environment Eval;

let process_input (state: Eval.evalStateT) ::cb in_str :unit =>
  switch (Parser.parse_single in_str) {
  | Ok e => Eval.eval e ctx::(Eval.create_initial_context state) ::state ::cb
  | Error _ as e => cb (e, state)
  };

let rec prompt state =>
  Readline.question
    rl
    "> "
    (
      process_input
        state
        cb::(
          fun (result, state) => {
            print_endline (Eval.AST.to_string result);
            prompt state
          }
        )
    );

prompt (Builtins.add_builtins Eval.empty);
