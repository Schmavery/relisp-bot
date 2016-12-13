open Common;

open Parse;

open Evaluate;

let received_error expected::expected args::args name::name state::state => (
  create_exception (
    "Received " ^
    string_of_int (List.length args) ^
    " arguments, expected " ^ string_of_int expected ^ " in call to '" ^ name ^ "'"
  ),
  state
);

let add_builtins state => {
  let add_native_lambda (name: string) macro::(is_macro: bool) func state::(state: Eval.t) :Eval.t => {
    let asyncf
        (args: list astNodeT)
        ctx::(ctx: ctxT)
        state::(state: Eval.t)
        cb::(cb: (result astNodeT astNodeT, Eval.t) => unit) =>
      func args ctx::ctx state::state |> cb;
    let node = create_node (NativeFunc {func: asyncf, is_macro});
    Eval.define_native_symbol state name node
  };
  let add_native_lambda_async
      (name: string)
      macro::(is_macro: bool)
      (func: nativeFuncT)
      state::(state: Eval.t)
      :Eval.t => {
    let node = create_node (NativeFunc {func, is_macro});
    Eval.define_native_symbol state name node
  };
  /* Define Builtins */
  let do_operation (op: float => float => float) (op_name: string) (state: Eval.t) :Eval.t =>
    add_native_lambda
      op_name
      state::state
      macro::false
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
                    | _ =>
                      create_exception ("Called '" ^ op_name ^ "' with something not a number.")
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
  let rec parse_lambda_args
          (args: list astNodeT)
          (acc: list string)
          :result (list string, option string) string =>
    switch args {
    | [] => Ok (List.rev acc, None)
    | [{value: Ident "..."}, {value: Ident vararg}] => Ok (List.rev acc, Some vararg)
    | [{value: Ident "..."}, ...tl] => Error "Can only have ... before the last argument name."
    | [{value: Ident ident}, ...tl] => parse_lambda_args tl [ident, ...acc]
    | _ => Error "Argument defined as not identifier in function definition."
    };
  let parse_lambda_args args => parse_lambda_args args [];
  let create_lambda_func is_macro args ctx::ctx state::state => {
    let uuid = gen_uuid ();
    switch args {
    | [{value: List args}, body] =>
      let arg_names = parse_lambda_args args;
      switch arg_names {
      | Error e => (create_exception e, state)
      | Ok (names, vararg) =>
        switch (create_lambda_scope body (StringMap.singleton "recur" uuid) names ctx state) {
        | Ok (idents, new_state) =>
          let node = {
            uuid,
            value: Func {func: body, args: names, scope: idents, is_macro, vararg}
          };
          /* add recur uuid */
          let new_state = Eval.add_to_uuid_map new_state node;
          (Ok node, new_state)
        | Error e => (create_exception e, state)
        }
      }
    | [_, _] => (create_exception "Expected list as first argument in call to 'lambda'", state)
    | args => received_error expected::2 args::args name::"lambda" state::state
    }
  };
  let state = add_native_lambda state::state "lambda" macro::true (create_lambda_func false);
  let state = add_native_lambda state::state "macro" macro::true (create_lambda_func true);
  let state =
    add_native_lambda
      state::state
      "throw"
      macro::false
      (
        fun args ctx::ctx state::state =>
          switch args {
          | [a] => (Error a, state)
          | lst => received_error expected::2 args::lst name::"throw" state::state
          }
      );
  let state =
    add_native_lambda_async
      state::state
      "try"
      macro::true
      (
        fun args ctx::ctx state::state cb::return =>
          switch args {
          | [first, lambda] =>
            Eval.eval
              first
              ctx::ctx
              state::state
              cb::(
                fun
                | (Error ex, state) =>
                  Eval.eval
                    lambda
                    ctx::ctx
                    state::state
                    cb::(
                      fun
                      | (Ok ({value: NativeFunc _} as func), state)
                      | (Ok ({value: Func _} as func), state) =>
                        Eval.eval_lambda
                          func args::[ex] ctx::ctx func_name::"[Try]" state::state cb::return
                      | (_, state) =>
                        return (
                          create_exception "Expected lambda as second argument in call to 'try'",
                          state
                        )
                    )
                | x => return x
              )
          | lst => return (received_error expected::2 args::lst name::"try" state::state)
          }
      );
  let state =
    add_native_lambda_async
      state::state
      "define"
      macro::true
      (
        fun args ctx::ctx state::state cb::return =>
          switch args {
          | [{value: Ident ident}, other] =>
            if (ctx.depth > 2) {
              return (create_exception "Can only define at the top level", state)
            } else if (
              not (Eval.is_reserved_symbol state ident)
            ) {
              return (create_exception ("Cannot define reserved keyword [" ^ ident ^ "]."), state)
            } else {
              Eval.eval
                other
                ctx::ctx
                state::state
                cb::(
                  fun
                  | (Ok {value: NativeFunc _}, _) =>
                    return (
                      create_exception ("Cannot rename builtin function [" ^ ident ^ "]."),
                      state
                    )
                  | (Ok res, state) =>
                    return (Ok Eval.empty_node, Eval.define_user_symbol state ident res)
                  | (Error _, _) as e => return e
                )
            }
          | [_, _] =>
            return (create_exception "Expected ident as first argument in call to 'define'", state)
          | lst => return (received_error expected::2 args::lst name::"define" state::state)
          }
      );
  let state =
    add_native_lambda
      state::state
      "quote"
      macro::true
      (
        fun args ctx::ctx state::state =>
          switch args {
          | [el] => (Ok el, state)
          | lst => received_error expected::1 args::lst name::"quote" state::state
          }
      );
  let state =
    add_native_lambda_async
      state::state
      "unquote"
      macro::true
      (
        fun args ctx::ctx state::state cb::return =>
          switch args {
          | [el] => Eval.eval el ctx::ctx state::state cb::return
          | lst => return (received_error expected::1 args::lst name::"unquote" state::state)
          }
      );
  let rec traverseH
          (lst: list astNodeT)
          (acc: list astNodeT)
          ctx::ctx
          state::state
          cb::(return: (result (list astNodeT) astNodeT, Eval.t) => unit)
          :unit =>
    switch lst {
    | [] => return (Ok acc, state)
    | [v, ...tl] =>
      /* switch acc {
         | (Error _, _) => return acc
         | (Ok acc_lst, state) => */
      switch v {
      | {value: List [{value: Ident "unquote-splice"}, unquote_arg]} =>
        Eval.eval
          unquote_arg
          ctx::ctx
          state::state
          cb::(
            fun (res, state) =>
              switch res {
              | Error e => return (Error e, state)
              | Ok {value: List lst} => traverseH tl (acc @ lst) ctx::ctx state::state cb::return
              | Ok _ => return (create_exception "'unquote-splice' only applies to lists", state)
              }
          )
      | v =>
        traverse
          v
          ctx::ctx
          state::state
          cb::(
            fun (res, state) =>
              switch res {
              | Error _ as e => return (e, state)
              | Ok node => traverseH tl (acc @ [node]) ctx::ctx state::state cb::return
              }
          )
      }
    /* } */
    /* | [] => return (ok ) */
    }
  and traverse
      (node: astNodeT)
      ctx::ctx
      state::(state: Eval.t)
      cb::(return: (result astNodeT astNodeT, Eval.t) => unit)
      :unit =>
    switch node {
    | {value: List [{value: Ident "unquote"}, next]} =>
      Eval.eval next ctx::ctx state::state cb::return
    | {value: List [{value: Ident "unquote"}, ...lst]} =>
      return (received_error expected::1 args::lst name::"unquote" state::state)
    | {value: List lst} =>
      /* let res = List.fold_left (traverseH ctx::ctx) (Ok [], state) lst; */
      traverseH
        lst
        []
        state::state
        ctx::ctx
        cb::(
          fun
          | (Ok a, state) => return (Ok (create_node (List a)), state)
          | (Error a, state) => return (Error a, state)
        )
    | a => return (Ok a, state)
    };
  let state =
    add_native_lambda_async
      state::state
      "syntax-quote"
      macro::true
      (
        fun args ctx::ctx state::state cb::return =>
          switch args {
          | [e] => traverse e ctx::ctx state::state cb::return
          | lst => return (received_error expected::1 args::lst name::"syntax-quote" state::state)
          }
      );
  let state =
    add_native_lambda_async
      state::state
      "if"
      macro::true
      (
        fun args ctx::ctx state::state cb::return =>
          switch args {
          | [cond, consequent, alternate] =>
            Eval.eval
              cond
              state::state
              ctx::ctx
              cb::(
                fun evaled_cond =>
                  switch (evaled_cond, consequent, alternate) {
                  | ((Error _, _) as e, _, _) => return e
                  | ((Ok {value: Bool true}, state), eval_me, _)
                  | ((Ok {value: Bool false}, state), _, eval_me) =>
                    Eval.eval eval_me state::state ctx::ctx cb::return
                  | ((Ok _, state), _, _) =>
                    return (
                      create_exception "First argument to 'if' must evaluate to boolean.",
                      state
                    )
                  }
              )
          | lst => return (received_error expected::3 args::lst name::"if" state::state)
          }
      );

  /** Note: load will later have to be brought out to have context of the
      environment it's running in to know how to load libs. */
  let read_lib (name: string) cb::cb => {
    let ic = open_in ("lib/" ^ name ^ ".lib");
    let try_read () =>
      try (Some (input_line ic)) {
      | End_of_file => None
      };
    let rec loop acc =>
      switch (try_read ()) {
      | Some s => loop [s, ...acc]
      | None =>
        close_in ic;
        List.rev acc
      };
    loop [] |> String.concat "" |> cb
  };
  let rec eval_list (lst: list astNodeT) (state: Eval.t) (ctx: ctxT) cb::return =>
    switch lst {
    | [] => return (Ok state)
    | [hd, ...tl] =>
      Eval.eval
        hd
        state::state
        ctx::ctx
        cb::(
          fun
          | (Ok _, state) => eval_list tl state ctx cb::return
          | (Error e, _) => return (Error e)
        )
    };
  let state =
    add_native_lambda_async
      state::state
      "load"
      macro::true
      (
        fun args ctx::ctx state::state cb::return =>
          switch args {
          | [{value: Str lib_name}] =>
            read_lib
              lib_name
              cb::(
                fun text =>
                  switch (Parse.parse_multi text) {
                  | Ok lst =>
                    eval_list
                      lst
                      state
                      ctx
                      cb::(
                        fun
                        | Ok state => return (Ok Eval.empty_node, state)
                        | Error e => return (Error e, state)
                      )
                  | Error e => return (Error e, state)
                  }
              )
          | lst => return (received_error expected::1 args::lst name::"load" state::state)
          }
      );
  state
};
