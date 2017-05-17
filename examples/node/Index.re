open Common;

open Evaluate;

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

module Builtins = BuiltinFuncs.Builtins Environment;

type rlPipeT;

type rlInterfaceDefT;

type rlInterfaceT;

external createRLInterfaceDef :
  input::rlPipeT => output::rlPipeT => rlInterfaceDefT =
  "" [@@bs.obj];

external createRLInterface : rlInterfaceDefT => rlInterfaceT =
  "createInterface" [@@bs.module "readline"];

external rlQuestion : rlInterfaceT => string => (string => unit) => unit =
  "question" [@@bs.send];

external stdin : rlPipeT = "process.stdin" [@@bs.val];

external stdout : rlPipeT = "process.stdout" [@@bs.val];

let rlDef = createRLInterfaceDef input::stdin output::stdout;

let rl = createRLInterface rlDef;

Random.self_init ();

let process_input (state: Eval.t) ::cb in_str :unit =>
  switch (Parse.parse_single in_str) {
  | Ok e => Eval.eval e ctx::(Eval.create_initial_context state) ::state ::cb
  | Error _ as e => cb (e, state)
  };

let rec prompt state =>
  rlQuestion
    rl
    "> "
    (
      process_input
        state
        cb::(
          fun (result, state) => {
            print_endline (string_of_ast result);
            prompt state
          }
        )
    );

prompt (Builtins.add_builtins Eval.empty);
