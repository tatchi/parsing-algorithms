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

type statement =
  | ExpressionStatement(expressionStatement)
and expressionStatement =
  | Expression(expression)
and expression =
  | NumericLiteral(int)
  | BinaryExpression(binaryExpression)
and binaryExpression = {
  op: binOp,
  left: expression,
  right: expression,
};

type t = statement;

let rec expr_to_json = exp => {
  switch (exp) {
  | NumericLiteral(n) =>
    `Assoc([("type", `String("NumericLiteral")), ("value", `Int(n))])
  | BinaryExpression(binExp) =>
    `Assoc([
      ("type", `String("BinaryExpression")),
      ("op", `String(string_of_binOp(binExp.op))),
      ("left", expr_to_json(binExp.left)),
      ("right", expr_to_json(binExp.right)),
    ])
  };
};

let expressionStatement_to_json = exprStatement => {
  switch (exprStatement) {
  | Expression(exp) => expr_to_json(exp)
  };
};

let toJson = statement =>
  switch (statement) {
  | ExpressionStatement(exprStatement) =>
    `Assoc([
      ("type", `String("ExpressionStatement")),
      ("expression", expressionStatement_to_json(exprStatement)),
    ])
  };

let toString = statement => {
  let json = toJson(statement);
  Yojson.Safe.pretty_to_string(json);
};
