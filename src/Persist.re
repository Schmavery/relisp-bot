let to_json: 'a => Js.Json.t = [%bs.raw
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
          return [o.hasOwnProperty("tag") ? o.tag : -1, o.map(toJSON)]);
        }
        throw new Error("Cannot serialize unidentified object [" + o + "].")
    }
  }
|}
];

let from_json: Js.Json.t => 'a = [%bs.raw
  {|
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
|}
];

let to_string (node: Common.AST.astNodeT) => Js.Json.stringify (to_json node);

let from_string (s: string) :Common.AST.astNodeT =>
  from_json (Js.Json.parseExn s);
