type errorT;

external readFile :
  string =>
  _ [@bs.as "utf8"] =>
  (errorT => Js.Undefined.t string => unit) =>
  unit =
  "" [@@bs.module "fs"];

let load_lib filename ::cb =>
  readFile
    ("stdlib/" ^ filename ^ ".bot")
    (fun _ str => cb (Js.Undefined.to_opt str));
