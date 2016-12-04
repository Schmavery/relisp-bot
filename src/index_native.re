open Common;

open Parse;

open Evaluate;

let process_input (in_str: string) (state: Eval.t) :(string, Eval.t) =>
  switch (parse in_str) {
  | Ok e =>
    let (res, state) = Eval.eval e ctx::(Eval.create_initial_context state) state::state;
    (string_of_ast res, state)
  | Error _ as e => (string_of_ast e, state)
  };

let rec main (state: Eval.t) => {
  let in_str = input_line stdin;
  if (in_str != "exit") {
    let (out_str, state) = process_input in_str state;
    print_endline out_str;
    main state
  }
};

Random.self_init ();

main (Builtins.add_builtins Eval.empty);
