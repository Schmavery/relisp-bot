module Environment = {
  type errorT;
  external readFile :
    string =>
    _ [@bs.as "utf8"] =>
    (errorT => Js.Undefined.t string => unit) =>
    unit =
    "" [@@bs.module "fs"];
  let load_lib filename ::cb =>
    readFile
      ("stdlib/" ^ filename ^ ".bot")
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

module AST = Common.AST;

module Eval = Evaluate.Eval;

module Builtins = BuiltinFuncs.Builtins Environment;

let process_input (state: AST.evalStateT) ::cb in_str :unit =>
  switch (Parse.Parser.parse_single in_str) {
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
          fun (result, state: AST.evalStateT) => {
            print_endline (AST.to_string result state);
            prompt {...state, recentActions: []}
          }
        )
    );

let state = Builtins.add_builtins Eval.empty;

switch (Parse.Parser.parse_single "(load \"std\")") {
| Ok e =>
  Eval.eval
    e
    ctx::(Eval.create_initial_context state)
    ::state
    cb::(
      fun (res, s) =>
        switch res {
        | Ok _ =>
          print_endline "Stdlib autoloaded successfully.";
          prompt s
        | Error _ =>
          print_endline (AST.to_string res state);
          print_endline "Error evaluating stdlib, continuing...";
          prompt state
        }
    )
| Error _ as e =>
  print_endline (AST.to_string e state);
  print_endline "Error parsing stdlib, continuing...";
  prompt state
};
