open Common;

open Evaluate;

module Stream = {
  type t = list char;
  let empty = [];
  let peek (stream: t) :option char =>
    switch stream {
    | [] => None
    | [c, ..._] => Some c
    };
  let pop (stream: t) :t =>
    switch stream {
    | [] => raise (Failure "Empty")
    | [_, ...lst] => lst
    };
  let create (s: string) :t => {
    let rec explode (i: int) (acc: list char) :t =>
      if (i < 0) {
        acc
      } else {
        explode (i - 1) [s.[i], ...acc]
      };
    explode (String.length s - 1) []
  };
};

type parseResult =
  | ParseOk (Stream.t, astNodeT)
  | ParseFail string
  | UnexpectedEnd;

let append_char (s: string) (c: char) :string => s ^ String.make 1 c;


/** atom parsers **/
let rec parse_string (stream: Stream.t) (acc: string) :parseResult =>
  switch (Stream.peek stream) {
  | Some '\\' =>
    let pop_str = Stream.pop stream;
    switch (Stream.peek pop_str) {
    | Some 'n' => parse_string (Stream.pop pop_str) (append_char acc '\n')
    | Some 't' => parse_string (Stream.pop pop_str) (append_char acc '\t')
    | Some '"' => parse_string (Stream.pop pop_str) (append_char acc '"')
    | Some '\'' => parse_string (Stream.pop pop_str) (append_char acc '\'')
    | Some '\\' => parse_string (Stream.pop pop_str) (append_char acc '\\')
    | Some c => ParseFail ("Invalid escape sequence \\" ^ append_char "" c ^ ".")
    | None => ParseFail "Unterminated string."
    }
  | Some '"' => ParseOk (Stream.pop stream, create_node (Str acc))
  | Some c => parse_string (Stream.pop stream) (append_char acc c)
  | None => ParseFail "Unterminated string."
  };

let rec parse_ident (stream: Stream.t) (acc: string) :parseResult =>
  switch (Stream.peek stream) {
  | Some ' '
  | Some '\t'
  | Some '\n'
  | Some '('
  | Some ')'
  | Some '"'
  | None =>
    switch acc {
    | "true" => ParseOk (stream, Eval.trueNode)
    | "false" => ParseOk (stream, Eval.falseNode)
    | "" => failwith "Parsed empty identifier"
    | _ => ParseOk (stream, create_node (Ident acc))
    }
  | Some c => parse_ident (Stream.pop stream) (append_char acc c)
  };

let rec parse_num (stream: Stream.t) (acc: string) :parseResult =>
  switch (Stream.peek stream) {
  | Some ('1'..'9' as c) => parse_num (Stream.pop stream) (append_char acc c)
  | _ =>
    let num =
      try (Some (float_of_string acc)) {
      | _ => None
      };
    switch num {
    | Some f => ParseOk (stream, create_node (Num f))
    | None => ParseFail ("Could not parse number [" ^ acc ^ "].")
    }
  };


/** Mututally recursive parse functions **/
let rec parse (stream: Stream.t) :parseResult =>
  switch (Stream.peek stream) {
  | Some '"' => parse_string (Stream.pop stream) ""
  | Some '(' => parse_list (Stream.pop stream) []
  | Some ' '
  | Some '\t'
  | Some '\n' =>
    print_endline "popped space";
    parse (Stream.pop stream)
  | Some '1'..'9' => parse_num stream ""
  | Some c => parse_ident stream ""
  | None => UnexpectedEnd
  }
and parse_list (stream: Stream.t) (acc: list astNodeT) :parseResult =>
  switch (Stream.peek stream) {
  | Some ')' => ParseOk (Stream.pop stream, create_node (List (List.rev acc)))
  | Some ' '
  | Some '\t'
  | Some '\n' => parse_list (Stream.pop stream) acc
  | Some c =>
    switch (parse stream) {
    | ParseOk (res_stream, res) => parse_list res_stream [res, ...acc]
    | UnexpectedEnd => ParseFail "Unterminated list."
    | ParseFail _ as e => e
    }
  | None => ParseFail "Unterminated list."
  };

let rec parse_multi (stream: Stream.t) (acc: list astNodeT) :result (list astNodeT) astNodeT =>
  switch (Stream.peek stream) {
  | Some _ =>
    switch (parse stream) {
    | ParseOk (stream, node) => parse_multi stream [node, ...acc]
    | ParseFail error => create_exception error
    | UnexpectedEnd => create_exception "Unexpected end of input."
    }
  | None => Ok (List.rev acc)
  };

let parse_multi (s: string) :result (list astNodeT) astNodeT => parse_multi (Stream.create s) [];

let parse_single s :result astNodeT astNodeT => {
  let stream = Stream.create s;
  switch (parse stream) {
  | ParseOk (s, node) =>
    switch (Stream.peek s) {
    | None => Ok node
    | Some c => create_exception ("Unexpected character " ^ append_char "" c ^ ".")
    }
  | ParseFail error => create_exception error
  | UnexpectedEnd => create_exception "Unexpected end of input."
  }
};
