type t;

type listenMsgT;

type threadInfoT;

type sendMsgInfoT;

type userInfosT;

type userInfoT;

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

external sendMessageCb :
  t =>
  msg::string =>
  id::string =>
  (errT => Js.Null.t sendMsgInfoT => unit) =>
  unit =
  "" [@@bs.send];

external listen : t => (errT => Js.Null.t listenMsgT => unit) => unit =
  "" [@@bs.send];

external setMessageReaction : t => emote::string => id::string => unit =
  "" [@@bs.send];

external getThreadInfo :
  t => id::string => (errT => Js.Null.t threadInfoT => unit) => unit =
  "" [@@bs.send];

external getUserInfo :
  t => id::array string => (errT => Js.Null.t userInfosT => unit) => unit =
  "" [@@bs.send];

/* Getters */
external getThreadID : listenMsgT => string = "threadID" [@@bs.get];

external getMessageID : listenMsgT => string = "messageID" [@@bs.get];

external getBody : listenMsgT => string = "body" [@@bs.get];

external getParticipantIDs : threadInfoT => array string =
  "participantIDs" [@@bs.get];

external getName : userInfoT => string = "name" [@@bs.get];

/* Helpers */
let unwrapUserInfo: userInfosT => array userInfoT = [%bs.raw
  {|function(i){Object.keys(i).map(k => i[k])}|}
];
