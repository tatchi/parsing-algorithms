type binOp =
  | BinOpPlus
  | BinOpMinus
  | BinOpMult
  | BinOpDiv
  | BinOpLessThan
  | BinOpLessThanEq
  | BinOpGreaterThan
  | BinOpGreaterThanEq
  | BinOpEq
  | BinOpNotEq
  | BinOpAnd
  | BinOpOr;

let string_of_binOp = binOp =>
  switch (binOp) {
  | BinOpPlus => "+"
  | BinOpMinus => "-"
  | BinOpMult => "*"
  | BinOpDiv => "/"
  | BinOpLessThan => "<"
  | BinOpLessThanEq => "<="
  | BinOpGreaterThan => ">"
  | BinOpGreaterThanEq => ">="
  | BinOpEq => "=="
  | BinOpNotEq => "!="
  | BinOpAnd => "&&"
  | BinOpOr => "||"
  };

type program =
  | Program(list(statement))
and statement =
  | ExpressionStatement(expressionStatement)
  | BlockStatement(blockStatement)
  | FunctionDeclaration(identifier, params, blockStatement)
  | ReturnStatement(option(expression))
  | IfStatement(expression, statement, option(statement))
  | EmptyStatement
and expressionStatement =
  | Expression(expression)
and expression =
  | Literal(literal)
  | Identifier(string)
  | BinaryExpression(binaryExpression)
  | LogicalExpression(binaryExpression)
and binaryExpression = {
  op: binOp,
  left: expression,
  right: expression,
}
and literal =
  | NumericLiteral(int)
  | BooleanLiteral(bool)
  | NullLiteral
and identifier = string
and params = list(identifier)
and blockStatement = list(statement);

type t = program;

let literal_to_json = literal =>
  switch (literal) {
  | NumericLiteral(n) =>
    `Assoc([("type", `String("NumericLiteral")), ("value", `Int(n))])
  | BooleanLiteral(b) =>
    `Assoc([("type", `String("BooleanLiteral")), ("value", `Bool(b))])
  | NullLiteral =>
    `Assoc([("type", `String("NullLiteral")), ("value", `Null)])
  };

let identifier_to_json = identifier =>
  `Assoc([
    ("type", `String("Identifier")),
    ("value", `String(identifier)),
  ]);

let rec expr_to_json = exp => {
  switch (exp) {
  | Literal(lit) => literal_to_json(lit)
  | Identifier(id) => identifier_to_json(id)
  | BinaryExpression(binExp) =>
    `Assoc([
      ("type", `String("BinaryExpression")),
      ("operator", `String(string_of_binOp(binExp.op))),
      ("left", expr_to_json(binExp.left)),
      ("right", expr_to_json(binExp.right)),
    ])
  | LogicalExpression(logicalExpr) =>
    `Assoc([
      ("type", `String("LogicalExpression")),
      ("operator", `String(string_of_binOp(logicalExpr.op))),
      ("left", expr_to_json(logicalExpr.left)),
      ("right", expr_to_json(logicalExpr.right)),
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
  | IfStatement(test, consequent, maybeAlternate) =>
    `Assoc([
      ("type", `String("IfStatement")),
      ("test", expr_to_json(test)),
      ("consequent", statement_to_json(consequent)),
      (
        "alternate",
        switch (maybeAlternate) {
        | None => `Null
        | Some(alternate) => statement_to_json(alternate)
        },
      ),
    ])
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
