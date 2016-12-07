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
  let add_native_lambda
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
    add_native_lambda
      state::state
      "try"
      macro::true
      (
        fun args ctx::ctx state::state =>
          switch args {
          | [first, lambda] =>
            switch (Eval.eval first ctx::ctx state::state) {
            | (Error ex, state) =>
              switch (Eval.eval lambda ctx::ctx state::state) {
              | (Ok ({value: NativeFunc _} as func), state)
              | (Ok ({value: Func _} as func), state) =>
                Eval.eval_lambda func args::[ex] ctx::ctx func_name::"[Try]" state::state
              | (_, state) => (
                  create_exception "Expected lambda as second argument in call to 'try'",
                  state
                )
              }
            | x => x
            }
          | lst => received_error expected::2 args::lst name::"try" state::state
          }
      );
  let state =
    add_native_lambda
      state::state
      "define"
      macro::true
      (
        fun args ctx::ctx state::state =>
          switch args {
          | [{value: Ident ident}, other] =>
            if (ctx.depth > 2) {
              (create_exception "Can only define at the top level", state)
            } else if (
              not (Eval.is_reserved_symbol state ident)
            ) {
              (create_exception ("Cannot define reserved keyword [" ^ ident ^ "]."), state)
            } else {
              switch (Eval.eval other ctx::ctx state::state) {
              | (Ok {value: NativeFunc _}, _) => (
                  create_exception ("Cannot rename builtin function [" ^ ident ^ "]."),
                  state
                )
              | (Ok res, state) => (Ok Eval.empty_node, Eval.define_user_symbol state ident res)
              | (Error _, _) as e => e
              }
            }
          | [_, _] => (
              create_exception "Expected ident as first argument in call to 'define'",
              state
            )
          | lst => received_error expected::2 args::lst name::"define" state::state
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
    add_native_lambda
      state::state
      "unquote"
      macro::true
      (
        fun args ctx::ctx state::state =>
          switch args {
          | [el] => Eval.eval el ctx::ctx state::state
          | lst => received_error expected::1 args::lst name::"unquote" state::state
          }
      );
  let rec traverseH acc (v: astNodeT) ctx::ctx :(result (list astNodeT) astNodeT, Eval.t) =>
    switch acc {
    | (Error _, _) => acc
    | (Ok acc_lst, state) =>
      switch v {
      | {value: List [{value: Ident "unquote-splice"}, unquote_arg]} =>
        let (res, state) as evaled = Eval.eval unquote_arg ctx::ctx state::state;
        switch res {
        | Error e => (Error e, state)
        | Ok {value: List lst} => (Ok (acc_lst @ lst), state)
        | Ok _ => (create_exception "'unquote-splice' only applies to lists", state)
        }
      | a =>
        let (res, state) = traverse a ctx::ctx state::state;
        switch res {
        | Error _ as e => (e, state)
        | Ok node => (Ok (acc_lst @ [node]), state)
        }
      }
    }
  and traverse (node: astNodeT) ctx::ctx state::state :(result astNodeT astNodeT, Eval.t) =>
    switch node {
    | {value: List [{value: Ident "unquote"}, next]} => Eval.eval next ctx::ctx state::state
    | {value: List [{value: Ident "unquote"}, ...lst]} =>
      received_error expected::1 args::lst name::"unquote" state::state
    | {value: List lst} =>
      let res = List.fold_left (traverseH ctx::ctx) (Ok [], state) lst;
      switch res {
      | (Ok a, state) => (Ok (create_node (List a)), state)
      | (Error a, state) => (Error a, state)
      }
    | a => (Ok a, state)
    };
  let state =
    add_native_lambda
      state::state
      "syntax-quote"
      macro::true
      (
        fun args ctx::ctx state::state =>
          switch args {
          | [e] => traverse e ctx::ctx state::state
          | lst => received_error expected::1 args::lst name::"syntax-quote" state::state
          }
      );
  let state =
    add_native_lambda
      state::state
      "if"
      macro::true
      (
        fun args ctx::ctx state::state =>
          switch args {
          | [cond, consequent, alternate] =>
            let evaled_cond = Eval.eval cond state::state ctx::ctx;
            switch (evaled_cond, consequent, alternate) {
            | ((Error _, _) as e, _, _) => e
            | ((Ok {value: Bool true}, state), eval_me, _)
            | ((Ok {value: Bool false}, state), _, eval_me) =>
              Eval.eval eval_me state::state ctx::ctx
            | ((Ok _, state), _, _) => (
                create_exception "First argument to 'if' must evaluate to boolean.",
                state
              )
            }
          | lst => received_error expected::3 args::lst name::"if" state::state
          }
      );

  /** Note: load will later have to be brought out to have context of the
      environment it's running in to know how to load libs. */
  let read_lines name :string => {
    let ic = open_in name;
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
    loop [] |> String.concat ""
  };
  let rec eval_list (lst: list astNodeT) (state: Eval.t) (ctx: ctxT) :result Eval.t astNodeT =>
    switch lst {
    | [] => Ok state
    | [hd, ...tl] =>
      switch (Eval.eval hd state::state ctx::ctx) {
      | (Ok _, state) => eval_list tl state ctx
      | (Error e, _) => Error e
      }
    };
  let state =
    add_native_lambda
      state::state
      "load"
      macro::true
      (
        fun args ctx::ctx state::state =>
          switch args {
          | [{value: Str lib_name}] =>
            switch (read_lines ("lib/" ^ lib_name) |> Parse.parse_multi) {
            | Ok lst =>
              switch (eval_list lst state ctx) {
              | Ok state => (Ok Eval.empty_node, state)
              | Error e => (Error e, state)
              }
            | Error e => (Error e, state)
            }
          | lst => received_error expected::1 args::lst name::"load" state::state
          }
      );
  state
};
