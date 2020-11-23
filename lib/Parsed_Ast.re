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
  | BlockStatement(blockStatement)
  | FunctionDeclaration(identifier, params, blockStatement)
  | ReturnStatement(option(expression))
  | EmptyStatement
and expressionStatement =
  | Expression(expression)
and expression =
  | NumericLiteral(int)
  | Identifier(string)
  | BinaryExpression(binaryExpression)
and binaryExpression = {
  op: binOp,
  left: expression,
  right: expression,
}
and identifier = string
and params = list(identifier)
and blockStatement = list(statement);

type t = program;

let identifier_to_json = identifier =>
  `Assoc([
    ("type", `String("Identifier")),
    ("value", `String(identifier)),
  ]);

let rec expr_to_json = exp => {
  switch (exp) {
  | NumericLiteral(n) =>
    `Assoc([("type", `String("NumericLiteral")), ("value", `Int(n))])
  | Identifier(id) => identifier_to_json(id)
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

let rec blockStatement_to_json = statementList =>
  `Assoc([
    ("type", `String("BlockStatement")),
    ("body", `List(statementList |> List.map(statement_to_json))),
  ])
and statement_to_json = statement =>
  switch (statement) {
  | ExpressionStatement(exprStatement) =>
    `Assoc([
      ("type", `String("ExpressionStatement")),
      ("expression", expressionStatement_to_json(exprStatement)),
    ])
  | BlockStatement(statementList) => blockStatement_to_json(statementList)
  | FunctionDeclaration(name, params, statementList) =>
    `Assoc([
      ("type", `String("FunctionDeclaration")),
      ("name", identifier_to_json(name)),
      ("params", `List(params |> List.map(identifier_to_json))),
      ("body", blockStatement_to_json(statementList)),
    ])
  | ReturnStatement(maybeExpr) =>
    `Assoc([
      ("type", `String("ReturnStatement")),
      (
        "argument",
        switch (maybeExpr) {
        | Some(expr) => expr_to_json(expr)
        | None => `Null
        },
      ),
    ])
  | EmptyStatement => `Assoc([("type", `String("EmptyStatement"))])
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
