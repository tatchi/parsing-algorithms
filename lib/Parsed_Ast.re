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

type program =
  | Program(list(statement))
and statement =
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

type t = program;

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

let statement_to_json = statement =>
  switch (statement) {
  | ExpressionStatement(exprStatement) =>
    `Assoc([
      ("type", `String("ExpressionStatement")),
      ("expression", expressionStatement_to_json(exprStatement)),
    ])
  };

let toJson = prog =>
  switch (prog) {
  | Program(statementList) =>
    `Assoc([
      ("type", `String("program")),
      ("body", `List(statementList |> List.map(statement_to_json))),
    ])
  };

let toString = program => {
  let json = toJson(program);
  Yojson.Safe.pretty_to_string(json);
};
