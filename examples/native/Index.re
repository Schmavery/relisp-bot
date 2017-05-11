open Common;

open Evaluate;
    /* let read_lib (name: string) ::cb => { */
    /*   let ic = open_in ("lib/" ^ name ^ ".lib"); */
    /*   let try_read () => */
    /*     try (Some (input_line ic)) { */
    /*     | End_of_file => None */
    /*     }; */
    /*   let rec loop acc => */
    /*     switch (try_read ()) { */
    /*     | Some s => loop [s, ...acc] */
    /*     | None => */
    /*       close_in ic; */
    /*       List.rev acc */
    /*     }; */
    /*   loop [] |> String.concat "" |> cb */
    /* }; */

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
