type pipeT;

type interfaceDefT;

type interfaceT;

external createInterfaceDef : input::pipeT => output::pipeT => interfaceDefT =
  "" [@@bs.obj];

external createInterface : interfaceDefT => interfaceT =
  "createInterface" [@@bs.module "readline"];

external question : interfaceT => string => (string => unit) => unit =
  "question" [@@bs.send];

external stdin : pipeT = "process.stdin" [@@bs.val];

external stdout : pipeT = "process.stdout" [@@bs.val];
