open Common;

let module Stream = {
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

let parse s => {
  let is_num c => Char.code c >= Char.code '0' && Char.code c <= Char.code '9';

  /** atom parsers **/
  let rec parse_string (stream: Stream.t) (acc: string) :parseResult =>
    switch (Stream.peek stream) {
    | Some '\\' =>
      let pop_str = Stream.pop stream;
      switch (Stream.peek pop_str) {
      | Some 'n' => parse_string (Stream.pop pop_str) (acc ^ String.make 1 '\n')
      | Some 't' => parse_string (Stream.pop pop_str) (acc ^ String.make 1 '\t')
      | Some '"' => parse_string (Stream.pop pop_str) (acc ^ String.make 1 '"')
      | Some '\'' => parse_string (Stream.pop pop_str) (acc ^ String.make 1 '\'')
      | Some '\\' => parse_string (Stream.pop pop_str) (acc ^ String.make 1 '\\')
      | Some c => ParseFail ("Invalid escape sequence \\" ^ String.make 1 c ^ ".")
      | None => ParseFail "Unterminated string."
      }
    | Some '"' => ParseOk (Stream.pop stream, create_node (Str acc))
    | Some c => parse_string (Stream.pop stream) (acc ^ String.make 1 c)
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
    | None => ParseOk (stream, create_node (Ident acc))
    | Some c => parse_ident (Stream.pop stream) (acc ^ String.make 1 c)
    };
  let rec parse_num (stream: Stream.t) (acc: string) :parseResult =>
    switch (Stream.peek stream) {
    | Some c when is_num c => parse_num (Stream.pop stream) (acc ^ String.make 1 c)
    | _ =>
      let num = try (Some (float_of_string acc)) {
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
    | Some '\n' => parse (Stream.pop stream)
    | Some c when is_num c => parse_num stream ""
    | Some c => parse_ident stream ""
    | None => UnexpectedEnd
    }
  and parse_list (stream: Stream.t) (acc: list astNodeT) :parseResult =>
    switch (Stream.peek stream) {
    | Some ')' => ParseOk (Stream.pop stream, create_node (List (List.rev acc)))
    | Some c =>
      switch (parse stream) {
      | ParseOk (res_stream, res) => parse_list res_stream [res, ...acc]
      | UnexpectedEnd => ParseFail "Unterminated list."
      | ParseFail _ as e => e
      }
    | None => ParseFail "Unterminated list."
    };
  let stream = Stream.create s;
  switch (parse stream) {
  | ParseOk (_, node) => Ok node
  | ParseFail error => create_exception error
  | UnexpectedEnd => create_exception "Unexpected end of input."
  }
};
