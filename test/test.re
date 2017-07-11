type readErrorT;

external readFile :
  string =>
  _ [@bs.as "utf8"] =>
  (readErrorT => Js.Undefined.t string => unit) =>
  unit =
  "" [@@bs.module "fs"];

let load_test filename ::cb =>
  readFile
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
  tests: Common.StringMap.t string
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
            fun m (k, json) =>
              switch (Js.Json.decodeString json) {
              | None => failwith "Test result must be string"
              | Some v => Common.StringMap.add k v m
              }
          )
          Common.StringMap.empty
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

let run_parse_test input expected map => {
  let result = Common.AST.to_string (Parse.Parser.parse_single input);
  if (result != expected) {
    Common.StringMap.add input (expected, result) map
  } else {
    map
  }
};

let green = "\027[32m";

let red = "\027[31m";

let endcolor = "\027[0m";

let underline = "\027[4m";

let print_fail s => print_endline (red ^ s ^ endcolor);

let print_succeed s => print_endline (green ^ s ^ endcolor);

let print_header s => print_endline (underline ^ s ^ endcolor);

/* let run_eval_test test => failwith "Eval tests not implemented"; */
let run_test_section (section: testSectionT) => {
  print_header ("Test: " ^ section.name);
  let failed =
    switch section.kind {
    | Parse =>
      Common.StringMap.fold run_parse_test section.tests Common.StringMap.empty
    | Eval => failwith "Unimplemented"
    };
  switch (Common.StringMap.cardinal failed) {
  | 0 => print_succeed "All tests passed!\n"
  | numfailed =>
    print_fail (
      string_of_int numfailed ^
      " out of " ^
      string_of_int (Common.StringMap.cardinal section.tests) ^ " tests failed.\n"
    );
    Common.StringMap.iter
      (
        fun input (expected, result) =>
          print_endline (
            "Input: " ^
            input ^
            "\n Expected: " ^
            expected ^ "\n Received: " ^ red ^ result ^ endcolor ^ "\n"
          )
      )
      failed
  }
};

let run_test_file tests => Array.iter run_test_section tests;

load_test "parser" cb::(fun s => parse_test_file s |> run_test_file);
