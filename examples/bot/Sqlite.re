type t;

type err;

external database : string => t =
  "Database" [@@bs.new] [@@bs.module "sqlite3"];

external run : t => string => Js.t 'a => (err => unit) => unit =
  "" [@@bs.send];

external all :
  t => string => Js.t 'a => (err => Js.Undefined.t (array (array 'b)) => unit) => unit =
  "" [@@bs.send];

external get :
  t => string => Js.t 'a => (err => Js.Undefined.t 'b => unit) => unit =
  "" [@@bs.send];

external exec :
  t => string  => (err => unit) => unit =
  "" [@@bs.send];
