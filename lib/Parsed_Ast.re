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

type unaryOp =
  | UnaryOpPlus
  | UnaryOpMinus;

type assignmentOp =
  | AssignmentOpEq
  | AssignmentOpEqMult
  | AssignmentOpEqPlus
  | AssignmentOpEqMinus;

let string_of_assignmentOp = assignmentOp =>
  switch (assignmentOp) {
  | AssignmentOpEq => "="
  | AssignmentOpEqMult => "*="
  | AssignmentOpEqPlus => "+="
  | AssignmentOpEqMinus => "-="
  };

let string_of_unaryOp = unaryOp =>
  switch (unaryOp) {
  | UnaryOpPlus => "+"
  | UnaryOpMinus => "-"
  };

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
  | VariableStatement(list(declaration))
  | IterationStatement(iterationStatement)
  | EmptyStatement
and declaration =
  | VariableDeclaration({
      id: identifier,
      init: option(expression),
    })
and expressionStatement =
  | Expression(expression)
and expression =
  | Literal(literal)
  | Identifier(identifier)
  | BinaryExpression(binaryExpression)
  | UnaryExpression(unaryExpression)
  | CallExpression(callExpression)
  | LogicalExpression(binaryExpression)
  | AssignmentExpression(assignmentExpression)
  | LeftHandSideExpression(leftHandSideExpression)
and unaryExpression = {
  unaryOp,
  argument: expression,
}
and callExpression = {
  callee,
  arguments: list(expression),
}
and callee =
  | Callee_LeftHandSideExpression(leftHandSideExpression)
  | Callee_CallExpression(callExpression)
and assignmentExpression = {
  assignmentOp,
  assignmentLeft: leftHandSideExpression,
  assignmentRight: expression,
}
and leftHandSideExpression =
  | LHandSideMemberExpression(memberExpression)
and memberExpression =
  | MExpIdentifier(identifier)
  | MExpExpression({
      object_: memberExpression,
      property: memberExpressionProperty,
    })
and memberExpressionProperty =
  | MExpPropertyIdentifier(identifier)
  | MExpPropertyExp(expression)
and binaryExpression = {
  op: binOp,
  left: expression,
  right: expression,
}
and literal =
  | NumericLiteral(int)
  | BooleanLiteral(bool)
  | NullLiteral
  | StringLiteral(string)
and identifier = string
and params = list(identifier)
and blockStatement = list(statement)
and iterationStatement =
  | WhileStatement({
      test: expression,
      body: statement,
    })
  | ForStatement({
      init: expression,
      test: expression,
      update: expression,
      body: statement,
    });

type t = program;

let literal_to_json = literal =>
  switch (literal) {
  | NumericLiteral(n) =>
    `Assoc([("type", `String("NumericLiteral")), ("value", `Int(n))])
  | BooleanLiteral(b) =>
    `Assoc([("type", `String("BooleanLiteral")), ("value", `Bool(b))])
  | NullLiteral =>
    `Assoc([("type", `String("NullLiteral")), ("value", `Null)])
  | StringLiteral(s) =>
    `Assoc([("type", `String("StringLiteral")), ("value", `String(s))])
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
  | AssignmentExpression(assignExp) =>
    `Assoc([
      ("type", `String("AssignmentExpression")),
      ("operator", `String(string_of_assignmentOp(assignExp.assignmentOp))),
      ("left", leftHandSideExpression_to_json(assignExp.assignmentLeft)),
      ("right", expr_to_json(assignExp.assignmentRight)),
    ])
  | UnaryExpression(unaryExp) =>
    `Assoc([
      ("type", `String("UnaryExpression")),
      ("operator", `String(string_of_unaryOp(unaryExp.unaryOp))),
      ("argument", expr_to_json(unaryExp.argument)),
    ])
  | CallExpression(callExp) => calleExpression_to_json(callExp)
  | LeftHandSideExpression(exp) => leftHandSideExpression_to_json(exp)
  };
}

and callee_to_json = callee =>
  switch (callee) {
  | Callee_LeftHandSideExpression(expr) =>
    leftHandSideExpression_to_json(expr)
  | Callee_CallExpression(expr) => calleExpression_to_json(expr)
  }

and calleExpression_to_json = callExp =>
  `Assoc([
    ("type", `String("CallExpression")),
    ("callee", callee_to_json(callExp.callee)),
    ("arguments", `List(callExp.arguments |> List.map(expr_to_json))),
  ])

and memberExpression_to_json = mExp =>
  switch (mExp) {
  | MExpIdentifier(id) => identifier_to_json(id)
  | MExpExpression(exp) =>
    `Assoc([
      ("type", `String("MemberExpression")),
      ("object", memberExpression_to_json(exp.object_)),
      (
        "property",
        switch (exp.property) {
        | MExpPropertyIdentifier(id) => identifier_to_json(id)
        | MExpPropertyExp(e) => expr_to_json(e)
        },
      ),
    ])
  }

and leftHandSideExpression_to_json = (expr: leftHandSideExpression) =>
  switch (expr) {
  | LHandSideMemberExpression(mExp) => memberExpression_to_json(mExp)
  };

let expressionStatement_to_json = exprStatement => {
  switch (exprStatement) {
  | Expression(exp) => expr_to_json(exp)
  };
};

let declaration_to_json = declaration => {
  switch (declaration) {
  | VariableDeclaration(variableDeclaration) =>
    `Assoc([
      ("type", `String("VariableDeclaration")),
      ("id", identifier_to_json(variableDeclaration.id)),
      (
        "init",
        switch (variableDeclaration.init) {
        | None => `Null
        | Some(expression) => expr_to_json(expression)
        },
      ),
    ])
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
  | IterationStatement(iterationStatement) =>
    switch (iterationStatement) {
    | WhileStatement(statement) =>
      `Assoc([
        ("type", `String("WhileStatement")),
        ("test", expr_to_json(statement.test)),
        ("body", statement_to_json(statement.body)),
      ])
    | ForStatement(statement) =>
      `Assoc([
        ("type", `String("ForStatement")),
        ("init", expr_to_json(statement.init)),
        ("test", expr_to_json(statement.test)),
        ("update", expr_to_json(statement.update)),
        ("body", statement_to_json(statement.body)),
      ])
    }

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
  | VariableStatement(declarations) =>
    `Assoc([
      ("type", `String("VariableStatement")),
      (
        "declarations",
        `List(declarations |> List.map(declaration_to_json)),
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
