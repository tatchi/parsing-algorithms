type binOp =
  | BinOpPlus
  | BinOpMinus
  | BinOpMult
  | BinOpDiv;

let string_of_binOp = binOp =>
  switch (binOp) {
  | BinOpPlus => "+"
  | BinOpMinus => "-"
  | BinOpMult => "*"
  | BinOpDiv => "/"
  };

type expression =
  | NumericLiteral(int)
  | BinaryExpression(binaryExpression)
and binaryExpression = {
  op: binOp,
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
      ("op", `String(string_of_binOp(binExp.op))),
      ("left", toJson(binExp.left)),
      ("right", toJson(binExp.right)),
    ])
  };
};

let toString = exp => {
  let json = toJson(exp);
  Yojson.Safe.pretty_to_string(json);
};
