open Common;

open Parse;

open Evaluate;

external log : 'a => unit = "console.log" [@@bs.val];

external getElementById : string => Js.t 'a = "document.getElementById" [@@bs.val];

external setOnKeyDown : Js.t 'a => (Js.t 'b => Js.boolean) => unit = "onkeydown" [@@bs.set];

external getValue : Js.t 'a => string = "value" [@@bs.get];

external setValue : Js.t 'a => string => unit = "value" [@@bs.set];

external getKeyCode : Js.t 'a => int = "keyCode" [@@bs.get];

external setScrollTop : Js.t 'a => int => unit = "scrollTop" [@@bs.set];

external getScrollHeight : Js.t 'a => int = "scrollHeight" [@@bs.get];

external appendChild : Js.t 'a => Js.t 'b => unit = "appendChild" [@@bs.send];

external createElement : string => Js.t 'a = "document.createElement" [@@bs.val];

external setInnerHtml : Js.t 'a => string => unit = "innerHTML" [@@bs.set];

let scroll_bottom obj => setScrollTop obj (getScrollHeight obj);

Random.self_init ();

let process_input (in_str: string) (state: Eval.t) :(string, Eval.t) =>
  switch (parse in_str) {
  | Ok e =>
    let (res, state) = Eval.eval e ctx::(Eval.create_initial_context state) state::state;
    (string_of_ast res, state)
  | Error _ as e => (string_of_ast e, state)
  };

let input_element = getElementById "input";

let console_element = getElementById "console";

let add_console_element inner_text => {
  let new_node = createElement "div";
  setInnerHtml new_node inner_text;
  appendChild console_element new_node;
  scroll_bottom console_element
};

let state = ref (Builtins.add_builtins Eval.empty);

let history: ref (list string) = ref [];

let position = ref 0;

let moveHistory (by: int) :option string => {
  let new_pos = !position + by;
  if (new_pos < 1 || new_pos > List.length !history) {
    None
  } else {
    position := new_pos;
    Some (List.nth !history (new_pos - 1))
  }
};

setOnKeyDown
  input_element
  (
    fun e =>
      switch (getKeyCode e) {
      | 13 =>
        let in_str = getValue input_element;
        history := [in_str, ...!history];
        position := 0;
        let (out_str, new_state) = process_input in_str !state;
        state := new_state;
        setValue input_element "";
        add_console_element ("> " ^ in_str);
        add_console_element out_str;
        Js.false_
      | 38 /* UpArrow */ =>
        switch (moveHistory 1) {
        | Some line =>
          setValue input_element line;
          Js.false_
        | None => Js.false_
        }
      | 40 /* DownArrow */ =>
        switch (moveHistory (-1)) {
        | Some line =>
          setValue input_element line;
          Js.false_
        | None => Js.false_
        }
      | _ => Js.true_
      }
  );
