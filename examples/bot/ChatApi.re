type t;

type loginCredsT = Js.Json.t;

external createLoginCreds : email::string => password::string => loginCredsT =
  "" [@@bs.obj];

external readFileSync : string => _ [@bs.as "utf8"] => string =
  "" [@@bs.module "fs"];

let parseLoginCreds filename => readFileSync filename |> Js.Json.parseExn;

external login : loginCredsT => t = "facebook-chat-api" [@@bs.module];

external sendMessage : t => msg::string => id::string => unit = "" [@@bs.send];

external listen : t => (_ => string => unit) => unit = "" [@@bs.send];
