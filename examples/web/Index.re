open Common;

module Environment = {
  module XHR = {
    type t;
    external create : unit => t = "XMLHttpRequest" [@@bs.new];
    external open_ :
      t => string => string => _ [@bs.as {json|true|json}] => unit =
      "open" [@@bs.send];
    external send : t => unit = "send" [@@bs.send];
    external readyState : t => int = "readyState" [@@bs.get];
    external status : t => int = "status" [@@bs.get];
    external responseText : t => string = "responseText" [@@bs.get];
    external setOnReadyStateChange : t => (unit => unit) => unit =
      "onreadystatechange" [@@bs.set];
  };
  let load_lib filename ::cb => {
    let xhr = XHR.create ();
    XHR.setOnReadyStateChange
      xhr
      (
        fun () =>
          if (XHR.readyState xhr == 4 && XHR.status xhr == 200) {
            cb (Some (XHR.responseText xhr))
          } else if (
            XHR.readyState xhr == 4
          ) {
            cb None
          }
      );
    XHR.open_ xhr "GET" ("stdlib/" ^ filename ^ ".bot");
    XHR.send xhr
  };
};

external log : 'a => unit = "console.log" [@@bs.val];

external getElementById : string => Js.t 'a =
  "document.getElementById" [@@bs.val];

external setOnKeyDown : Js.t 'a => (Js.t 'b => Js.boolean) => unit =
  "onkeydown" [@@bs.set];

external getValue : Js.t 'a => string = "value" [@@bs.get];

external setValue : Js.t 'a => string => unit = "value" [@@bs.set];

external getKeyCode : Js.t 'a => int = "keyCode" [@@bs.get];

external setScrollTop : Js.t 'a => int => unit = "scrollTop" [@@bs.set];

external getScrollHeight : Js.t 'a => int = "scrollHeight" [@@bs.get];

external appendChild : Js.t 'a => Js.t 'b => unit = "appendChild" [@@bs.send];

external createElement : string => Js.t 'a =
  "document.createElement" [@@bs.val];

external setInnerHtml : Js.t 'a => string => unit = "innerHTML" [@@bs.set];

type styleT;

external getStyle : Js.t 'a => styleT = "style" [@@bs.get];

external setFontFamily : styleT => string => unit = "fontFamily" [@@bs.set];

type regexpT;

external regexp : string => string => regexpT = "RegExp" [@@bs.new];

external replace : string => regexpT => string => string = "" [@@bs.send];

let scroll_bottom obj => setScrollTop obj (getScrollHeight obj);

Random.self_init ();

module AST = Common.AST;

module Eval = Evaluate.Eval;

module Builtins = BuiltinFuncs.Builtins Environment;

let process_input (state: AST.evalStateT) ::cb in_str :unit =>
  switch (Parse.Parser.parse_single in_str) {
  | Ok e => Eval.eval e ctx::(Eval.create_initial_context state) ::state ::cb
  | Error _ as e => cb (e, state)
  };

let input_element = getElementById "input";

let console_element = getElementById "console";

let setMonospaced el => setFontFamily (getStyle el) "monospace";

setMonospaced input_element;

setMonospaced console_element;

let add_console_element inner_text => {
  let new_node = createElement "div";
  let inner_text = replace inner_text (regexp "\n" "g") "<br>";
  let inner_text = replace inner_text (regexp " " "g") "&nbsp;";
  setInnerHtml new_node inner_text;
  appendChild console_element new_node;
  scroll_bottom console_element
};

let state = ref (Builtins.add_builtins Eval.empty);

let evaluating = ref false;

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
      | 13 when not !evaluating =>
        let in_str = getValue input_element;
        evaluating := true;
        history := [in_str, ...!history];
        position := 0;
        process_input
          !state
          cb::(
            fun (res, new_state) => {
              evaluating := false;
              state := {...new_state, recentActions: []};
              setValue input_element "";
              add_console_element ("> " ^ in_str);
              AST.to_string res new_state |> add_console_element
            }
          )
          in_str;
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
