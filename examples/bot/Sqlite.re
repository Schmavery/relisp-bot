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

/* let db = database "tes" */

/* run */
/*   db */
/*   "CREATE TABLE test (info TEXT)" */
/*   (Js.Obj.empty ()) */
/*   (fun _ => print_endline "helooo"); */

/* module SM = Common.StringMap; */

/* let map = SM.empty; */
/* let map = SM.add "test1" "val1" map; */
/* let map = SM.add "test2" "val2" map; */
/* let map = SM.add "test3" "val3" map; */
/* let map = SM.add "test4" "val4" map; */
/* let map = SM.add "test5" "val5" map; */
/* let map = SM.add "test6" "val6" map; */
/* print_endline (SM.find "test3" map); */
/* let map = (Persist.from_string (Persist.to_string map)); */
/* print_endline (SM.find "test3" map); */
