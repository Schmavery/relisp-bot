type t;

type listenMsgT;

type errT;

type loginCredsT = Js.Json.t;

external createLoginCreds : email::string => password::string => loginCredsT =
  "" [@@bs.obj];

external readFileSync : string => _ [@bs.as "utf8"] => string =
  "" [@@bs.module "fs"];

let parseLoginCreds filename => readFileSync filename |> Js.Json.parseExn;

external login : loginCredsT => (errT => Js.Null.t t => unit) => unit =
  "facebook-chat-api" [@@bs.module];

external sendMessage : t => msg::string => id::string => unit = "" [@@bs.send];

external listen : t => (errT => Js.Null.t listenMsgT => unit) => unit = "" [@@bs.send];

external setMessageReaction : t => emote::string => id::string => unit = "" [@@bs.send];

external getThreadID : listenMsgT => string = "threadID" [@@bs.get];

external getMessageID : listenMsgT => string = "messageID" [@@bs.get];

external getBody : listenMsgT => string = "body" [@@bs.get];
