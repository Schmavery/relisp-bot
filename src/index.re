open Common;

open Parse;

open Evaluate;

let state: Eval.t = Eval.empty;

/* Define Builtins */
let do_operation (op: float => float => float) (op_name: string) (state:Eval.t) :Eval.t =>
  Eval.add_native_lambda
    op_name
    state::state
    false
    (
      fun args ctx::ctx state::state => (
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
        },
        state
      )
    );

let state = do_operation (+.) "+" state;

let state = do_operation (-.) "-" state;

let state = do_operation (/.) "/" state;

let state = do_operation ( *. ) "*" state;

let rec create_lambda_scope
        (node: astNodeT)
        (map: StringMap.t uuidT)
        (arg_names: list string)
        (ctx: ctxT)
        (state: Eval.t)
        :result (StringMap.t uuidT, Eval.t) string =>
  switch node {
  | {uuid, value: Ident i} =>
    switch (Eval.resolve_ident i ctx state) {
    | None => Ok (map, state)
    /* TODO: Intelligently parse body to statically check
       for undefined identifiers (idents could be captured by
       another lambda within the body of this lambda) */
    | Some x =>
      let new_state = Eval.add_to_uuid_map state x;
      Ok (StringMap.add i x.uuid map, new_state)
    }
  | {uuid, value: List lst} =>
    List.fold_left
      (
        fun acc v =>
          switch acc {
          | Ok (a, eval_state) => create_lambda_scope v a arg_names ctx eval_state
          | x => x
          }
      )
      (Ok (map, state))
      lst
  | {value: Num _}
  | {value: Str _}
  | {value: Bool _} => Ok (map, state)
  | _ => Error "Found non-literal in lambda body."
  };

let state =
  Eval.add_native_lambda
    state::state
    "lambda"
    true
    (
      fun args ctx::ctx state::state => {
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
          | Error e => (create_exception e, state)
          | Ok names =>
            switch (create_lambda_scope body (StringMap.singleton "recur" uuid) names ctx state) {
            | Ok (idents, new_state) =>
              let node = {
                uuid,
                value: Func {func: body, args: names, scope: idents, is_macro: false}
              };
              /* add recur uuid */
              let new_state = Eval.add_to_uuid_map new_state node;
              (Ok node, new_state)
            | Error e => (create_exception e, state)
            }
          }
        | [_, _] => (create_exception "Expected 2 lists in call to 'lambda'", state)
        | _ =>
          let received = string_of_int (List.length args);
          (
            create_exception (
              "Received " ^ received ^ " arguments, expected 2 in call to 'lambda'"
            ),
            state
          )
        }
      }
    );

let state =
  Eval.add_native_lambda
    state::state
    "throw"
    false
    (
      fun args ctx::ctx state::state => (
        switch args {
        | [a] => Error a
        | lst =>
          let received = string_of_int (List.length lst);
          create_exception ("Received " ^ received ^ " arguments, expected 2 in call to 'throw'")
        },
        state
      )
    );

let state =
  Eval.add_native_lambda
    state::state
    "try"
    true
    (
      fun args ctx::ctx state::state =>
        switch args {
        | [first, lambda] =>
          switch (Eval.eval first ctx::ctx state::state) {
          | (Error ex, state) =>
            switch (Eval.eval lambda ctx::ctx state::state) {
            | (Ok ({value: NativeFunc _} as func), state)
            | (Ok ({value: Func _} as func), state) =>
              Eval.eval_lambda func args::[ex] ctx::ctx func_name::"[Catch]" state::state
            | (_, state) => (
                create_exception "Expected lambda as second argument in call to 'catch'",
                state
              )
            }
          | x => x
          }
        | lst =>
          let received = string_of_int (List.length lst);
          (
            create_exception ("Received " ^ received ^ " arguments, expected 2 in call to 'catch'"),
            state
          )
        }
    );

let rec main (state: Eval.t) => {
  let in_str = input_line stdin;
  if (in_str != "exit") {
    switch (parse in_str) {
    | Ok e =>
      let (res, state) = Eval.eval e ctx::{argsUuidMap: StringMap.empty} state::state;
      string_of_ast res |> print_endline;
      main state
    | Error _ as e =>
      string_of_ast e |> print_endline;
      main state
    }
  }
};

Random.self_init ();

main state;
