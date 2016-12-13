open Common;

open Evaluate;

let process_input (in_str: string) (state: Eval.t) cb :unit =>
  switch (Parse.parse_single in_str) {
  | Ok e => Eval.eval e ctx::(Eval.create_initial_context state) state::state cb::cb
  | Error _ as e => cb (e, state)
  };

let rec main (state: Eval.t) =>
  switch (input_line stdin) {
  | "exit" => print_endline "Goodbye"
  | v =>
    process_input
      v
      state
      (
        fun (res, state) => {
          string_of_ast res |> print_endline;
          main state
        }
      )
  };

Random.self_init ();

main (Builtins.add_builtins Eval.empty);
