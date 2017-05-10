open Common;

open Evaluate;

type stdinT;

type indataT;

external openStdin : unit => stdinT = "process.openStdin" [@@bs.val];

external addDataListener : _ [@bs.as "data"] => (indataT => unit) => unit =
  "addListener" [@@bs.send.pipe : stdinT];

external toString : 'a => string = "toString" [@@bs.send];

let lastState = ref (Some (Builtins.add_builtins Eval.empty));

Random.self_init ();

let process_input (in_str: string) (state: Eval.t) ::cb :unit =>
  switch (Parse.parse_single in_str) {
  | Ok e => Eval.eval e ctx::(Eval.create_initial_context state) ::state ::cb
  | Error _ as e => cb (e, state)
  };

openStdin () |>
addDataListener (
  fun d => {
    let in_str = String.trim (toString d);
    switch !lastState {
    | None => print_endline ""
    | Some state =>
      lastState := None;
      process_input
        in_str
        state
        cb::(
          fun (res, new_state) => {
            lastState := Some new_state;
            string_of_ast res |> print_endline;
            print_string "> "
          }
        )
    }
  }
);

print_string "> "
