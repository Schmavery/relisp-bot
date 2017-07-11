[%%bs.raw
  {|
  function toJson(o) {
    switch (typeof o){
      case "boolean":
      case "number":
      case "string":
        return o;
      case "function":
        throw new Error("Cannot serialize functions");
      case "object":
        if (Array.isArray(o)){
          return [o.hasOwnProperty("tag") ? o.tag : -1, o.map(toJson)];
        }
        throw new Error("Cannot serialize unidentified object [" + o + "].")
    }
  }
  function fromJson(o) {
    switch (typeof o){
      case "boolean":
      case "number":
      case "string":
        return o;
      case "function":
        throw new Error("Cannot deserialize functions");
      case "object":
        if (Array.isArray(o)){
          var first = o[0]
          if (first == -1){
            return o[1].map(fromJson);
          } else {
            var a = o[1].map(fromJson);
            a.tag = first;
            return a
          }
        }
        throw new Error("Cannot deserialize unidentified object [" + o + "].")
    }
  }
  global.toJson = toJson;
  global.fromJson = fromJson;
|}
];

external to_json : 'a => Js.Json.t = "toJson" [@@bs.val] [@@bs.scope "global"];

external from_json : Js.Json.t => 'a = "fromJson" [@@bs.val] [@@bs.scope "global"];

let to_string o => Js.Json.stringify (to_json o);
let from_string s => from_json (Js.Json.parseExn s);
