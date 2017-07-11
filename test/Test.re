type readErrorT;

module StringMap = Common.StringMap;

module Environment = {
  type errorT;
  external readFile :
    string =>
    _ [@bs.as "utf8"] =>
    (errorT => Js.Undefined.t string => unit) =>
    unit =
    "" [@@bs.module "fs"];
  let load_lib filename ::cb =>
    readFile
      ("stdlib/" ^ filename ^ ".lib")
      (fun _ str => cb (Js.Undefined.to_opt str));
};

let load_test filename ::cb =>
  Environment.readFile
    ("test/" ^ filename ^ ".json")
    (
      fun _ str =>
        cb (
          switch (Js.Undefined.to_opt str) {
          | None => "Could not load test"
          | Some s => s
          }
        )
    );

type testTypeT =
  | Parse
  | Eval;

type testSectionT = {
  kind: testTypeT,
  name: string,
  tests: list (string, string)
};

[%%bs.raw
  {|
  // Object.entries polyfill because node
  // apparently doesn't have it
  if (!Object.entries){
      Object.entries = function (obj) {
	var entries = [];
	for (var key in obj) {
	  if (obj.hasOwnProperty(key)) {
	    entries.push([key, obj[key]]);
	  }
	}
	return entries;
      }
  }
|}
];

external objEntries : Js.Dict.t Js.Json.t => array (string, Js.Json.t) =
  "entries" [@@bs.scope "Object"] [@@bs.val];

let parse_test_section (section: Js.Json.t) =>
  switch (Js.Json.decodeObject section) {
  | None => failwith "Expected test section."
  | Some section =>
    switch (Js.Dict.get section "name") {
    | None => failwith "Expected test section name."
    | Some name =>
      let name =
        switch (Js.Json.decodeString name) {
        | None => failwith "Section name was not a string"
        | Some n => n
        };
      let typestr =
        switch (Js.Dict.get section "type") {
        | None => failwith "No section type defined"
        | Some x => x
        };
      let kind =
        switch (Js.Json.decodeString typestr) {
        | Some "parse" => Parse
        | Some "eval" => Eval
        | Some s => failwith ("Invalid section type " ^ s)
        | None => failwith "Section type was not a string"
        };
      let tests =
        switch (Js.Dict.get section "tests") {
        | None => failwith ("No tests defined for section " ^ name)
        | Some m =>
          switch (Js.Json.decodeObject m) {
          | None => failwith "Section tests should be an object"
          | Some m => m
          }
        };
      let tests =
        Array.fold_left
          (
            fun l (k, json) =>
              switch (Js.Json.decodeString json) {
              | None => failwith "Test result must be string"
              | Some v => [(k, v), ...l]
              }
          )
          []
          (objEntries tests);
      {kind, name, tests}
    }
  };

let parse_test_file (def: string) => {
  let json = Js.Json.parseExn def;
  switch (Js.Json.decodeArray json) {
  | None => failwith "Expected array of test sections."
  | Some arr => Array.map parse_test_section arr
  }
};

let green = "\027[32m";

let red = "\027[31m";

let endcolor = "\027[0m";

let underline = "\027[4m";

let print_fail s => print_endline (red ^ s ^ endcolor);

let print_succeed s => print_endline (green ^ s ^ endcolor);

let print_header s => print_endline (underline ^ s ^ endcolor);

let run_parse_test (input, expected) results cb :unit => {
  let result = Common.AST.to_string (Parse.Parser.parse_single input);
  if (result != expected) {
    cb [(input, expected, result), ...results]
  } else {
    cb results
  }
};

module Builtins = BuiltinFuncs.Builtins Environment;

module Eval = Evaluate.Eval;

let run_eval_test (input, expected) results cb :unit => {
  let state = Builtins.add_builtins Eval.empty;
  let ctx = Eval.create_initial_context state;
  switch (Parse.Parser.parse_single input) {
  | Ok parsed =>
    Eval.eval
      parsed
      ::ctx
      ::state
      cb::(
        fun (node, _state) => {
          let result = Common.AST.to_string node;
          if (result != expected) {
            cb [(input, expected, result), ...results]
          } else {
            cb results
          }
        }
      )
  | Error _ => failwith "Parse failure in eval test."
  }
};

let print_test_results (res: list (string, string, string)) section =>
  switch (List.length res) {
  | 0 => print_succeed "All tests passed!\n"
  | numfailed =>
    print_fail (
      string_of_int numfailed ^
      " out of " ^
      string_of_int (List.length section.tests) ^ " tests failed.\n"
    );
    List.iter
      (
        fun (input, expected, result) =>
          print_endline (
            "Input: " ^
            input ^
            "\n Expected: " ^
            expected ^ "\n Received: " ^ red ^ result ^ endcolor ^ "\n"
          )
      )
      res
  };

let rec fold_cb
        (fn: 'a => 'b => ('b => unit) => unit)
        (acc: 'b)
        (lst: list 'a)
        (cb: 'b => unit) =>
  switch lst {
  | [] => cb acc
  | [hd, ...tl] => fn hd acc (fun new_acc => fold_cb fn new_acc tl cb)
  };

let run_test_section (section: testSectionT) (numfail, numsuccess) cb => {
  let kindstr =
    switch section.kind {
    | Parse => "Parse"
    | Eval => "Eval"
    };
  print_endline (kindstr ^ ": " ^ underline ^ section.name ^ endcolor);
  let processor =
    switch section.kind {
    | Parse => run_parse_test
    | Eval => run_eval_test
    };
  fold_cb
    processor
    []
    section.tests
    (
      fun results => {
        print_test_results results section;
        cb (
          numfail + List.length results,
          numsuccess + List.length section.tests
        )
      }
    )
};

let print_summary cb (failed, total) =>
  if (failed == 0) {
    print_succeed (
      "\nTotal: All " ^ string_of_int total ^ " tests passed :)"
    )
  } else {
    print_fail (
      "\nTotal: " ^
      string_of_int failed ^
      " out of " ^ string_of_int total ^ " tests failed :("
    )
  };

let run_test_file tests cb =>
  fold_cb run_test_section (0, 0) tests (print_summary cb);

load_test
  "test"
  cb::(
    fun s => {
      let parsed_tests = parse_test_file s;
      run_test_file (Array.to_list parsed_tests) (fun _ => ())
    }
  );
