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
  let peek_second stream => peek (pop stream);
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

let append_char (s: string) (c: char) :string => s ^ String.make 1 c;

module Parser (AST: Common.AST_Type) => {
  module Constants = Common.Constants AST;
  type parseResult =
    | ParseOk (Stream.t, AST.astNodeT)
    | ParseFail string
    | UnexpectedEnd;

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
      | Some c =>
        ParseFail ("Invalid escape sequence \\" ^ append_char "" c ^ ".")
      | None => ParseFail "Unterminated string."
      }
    | Some '"' => ParseOk (Stream.pop stream, AST.create_node (Str acc))
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
    | Some ';'
    | None =>
      switch acc {
      | "true" => ParseOk (stream, Constants.true_node)
      | "false" => ParseOk (stream, Constants.false_node)
      | "" => ParseFail "Parsed empty identifier"
      | _ => ParseOk (stream, AST.create_node (Ident acc))
      }
    | Some c => parse_ident (Stream.pop stream) (append_char acc c)
    };
  let rec parse_num (stream: Stream.t) (acc: string) :parseResult =>
    switch (Stream.peek stream) {
    | Some ('0'..'9' as c) => parse_num (Stream.pop stream) (append_char acc c)
    | _ =>
      let num =
        try (Some (float_of_string acc)) {
        | _ => None
        };
      switch num {
      | Some f => ParseOk (stream, AST.create_node (Num f))
      | None => ParseFail ("Could not parse number [" ^ acc ^ "].")
      }
    };
  let rec pop_newline (stream: Stream.t) :Stream.t =>
    switch (Stream.peek stream) {
    | Some '\n' => Stream.pop stream
    | Some _ => pop_newline (Stream.pop stream)
    | None => stream
    };
  let wrap_quote_sugar ident_name inner_val =>
    switch inner_val {
    | ParseOk (stream, node) =>
      ParseOk (
        stream,
        AST.create_node (List [AST.create_node (Ident ident_name), node])
      )
    | ParseFail "Unexpected whitespace" =>
      ParseFail ("Unexpected whitespace after " ^ ident_name)
    | ParseFail _ as f => f
    | UnexpectedEnd => UnexpectedEnd
    };

  /** Mututally recursive parse functions **/
  let rec parse (stream: Stream.t) :parseResult =>
    switch (Stream.peek stream) {
    | Some ';' => parse (pop_newline (Stream.pop stream))
    | Some ' '
    | Some '\t'
    | Some '\n' => parse (Stream.pop stream)
    | Some '\'' =>
      wrap_quote_sugar
        "quote" (parse_no_leading_whitespace (Stream.pop stream))
    | Some '`' =>
      wrap_quote_sugar
        "syntax-quote" (parse_no_leading_whitespace (Stream.pop stream))
    | Some '~' =>
      switch (Stream.peek_second stream) {
      | Some '@' =>
        wrap_quote_sugar
          "unquote-splice"
          (parse_no_leading_whitespace (Stream.pop (Stream.pop stream)))
      | Some _ =>
        wrap_quote_sugar
          "unquote" (parse_no_leading_whitespace (Stream.pop stream))
      | None => UnexpectedEnd
      }
    | Some _ => parse_no_leading_whitespace stream
    | None => UnexpectedEnd
    }
  and parse_no_leading_whitespace (stream: Stream.t) :parseResult =>
    switch (Stream.peek stream) {
    | Some '"' => parse_string (Stream.pop stream) ""
    | Some '(' => parse_list (Stream.pop stream) []
    | Some '0'..'9' => parse_num stream ""
    | Some ';'
    | Some ' '
    | Some '\t'
    | Some '\n' => ParseFail "Unexpected whitespace"
    | Some ')' => ParseFail "Unexpected close paren"
    | Some _ => parse_ident stream ""
    | None => UnexpectedEnd
    }
  and parse_list (stream: Stream.t) (acc: list AST.astNodeT) :parseResult =>
    switch (Stream.peek stream) {
    | Some ')' =>
      ParseOk (Stream.pop stream, AST.create_node (List (List.rev acc)))
    | Some ';' => parse_list (pop_newline (Stream.pop stream)) acc
    | Some ' '
    | Some '\t'
    | Some '\n' => parse_list (Stream.pop stream) acc
    | Some _ =>
      switch (parse stream) {
      | ParseOk (res_stream, res) => parse_list res_stream [res, ...acc]
      | UnexpectedEnd => ParseFail "Unterminated list."
      | ParseFail _ as e => e
      }
    | None => ParseFail "Unterminated list."
    };
  let rec parse_multi
          (stream: Stream.t)
          (acc: list AST.astNodeT)
          :Common.result (list AST.astNodeT) AST.astNodeT =>
    switch (Stream.peek stream) {
    | Some _ =>
      switch (parse stream) {
      | ParseOk (stream, node) => parse_multi stream [node, ...acc]
      | ParseFail error => AST.create_exception error
      | UnexpectedEnd => AST.create_exception "Unexpected end of input."
      }
    | None => Ok (List.rev acc)
    };
  let parse_multi (s: string) :Common.result (list AST.astNodeT) AST.astNodeT =>
    parse_multi (Stream.create s) [];
  let rec trim_comments (stream: Stream.t) =>
    switch (Stream.peek stream) {
    | Some ';' => trim_comments (pop_newline (Stream.pop stream))
    | _ => stream
    };
  let parse_single s :Common.result AST.astNodeT AST.astNodeT => {
    let stream = Stream.create s;
    switch (parse stream) {
    | ParseOk (s, node) =>
      switch (Stream.peek (trim_comments s)) {
      | None => Ok node
      | Some c =>
        AST.create_exception (
          "Unexpected character [" ^ append_char "" c ^ "]."
        )
      }
    | ParseFail error => AST.create_exception error
    | UnexpectedEnd => AST.create_exception "Unexpected end of input."
    }
  };
};
