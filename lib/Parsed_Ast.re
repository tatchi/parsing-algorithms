type expression =
  | NumericLiteral(int)
  | BinaryExpression(binaryExpression)
and binaryExpression = {
  op: string,
  left: expression,
  right: expression,
};

type t = expression;

let rec toJson = exp => {
  switch (exp) {
  | NumericLiteral(n) =>
    `Assoc([("type", `String("NumericLiteral")), ("value", `Int(n))])
  | BinaryExpression(binExp) =>
    `Assoc([
      ("type", `String("BinaryExpression")),
      ("op", `String(binExp.op)),
      ("left", toJson(binExp.left)),
      ("right", toJson(binExp.right)),
    ])
  };
};

let toString = exp => {
  let json = toJson(exp);
  Yojson.Safe.pretty_to_string(json);
}
