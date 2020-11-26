let%expect_test "add two int" = {
  print_endline(Sys.getcwd())
  let parsed =
    Parser.program(Lexer.read_token, Lexing.from_channel(Stdlib.open_in("./data.txt")));
  let json = Parsed_Ast.toString(parsed);
  print_endline(json);
  %expect
  {|
    /Users/corentin/Documents/essentials-of-parsing/_build/default/lib
    {
      "type": "program",
      "body": [
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "BinaryExpression",
            "op": "+",
            "left": { "type": "NumericLiteral", "value": 2 },
            "right": { "type": "NumericLiteral", "value": 2 }
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "BinaryExpression",
            "op": "+",
            "left": { "type": "NumericLiteral", "value": 5 },
            "right": {
              "type": "BinaryExpression",
              "op": "*",
              "left": { "type": "NumericLiteral", "value": 3 },
              "right": { "type": "NumericLiteral", "value": 2 }
            }
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": { "type": "NumericLiteral", "value": 10 }
        },
        {
          "type": "BlockStatement",
          "body": [
            {
              "type": "ExpressionStatement",
              "expression": {
                "type": "BinaryExpression",
                "op": "+",
                "left": { "type": "NumericLiteral", "value": 5 },
                "right": { "type": "NumericLiteral", "value": 1 }
              }
            },
            {
              "type": "BlockStatement",
              "body": [
                {
                  "type": "ExpressionStatement",
                  "expression": {
                    "type": "BinaryExpression",
                    "op": "+",
                    "left": { "type": "NumericLiteral", "value": 1 },
                    "right": { "type": "NumericLiteral", "value": 1 }
                  }
                }
              ]
            }
          ]
        },
        { "type": "BlockStatement", "body": [] },
        {
          "type": "FunctionDeclaration",
          "name": { "type": "Identifier", "value": "myFn" },
          "params": [
            { "type": "Identifier", "value": "a" },
            { "type": "Identifier", "value": "b" }
          ],
          "body": {
            "type": "BlockStatement",
            "body": [
              {
                "type": "ExpressionStatement",
                "expression": {
                  "type": "BinaryExpression",
                  "op": "+",
                  "left": { "type": "NumericLiteral", "value": 1 },
                  "right": { "type": "NumericLiteral", "value": 2 }
                }
              },
              {
                "type": "ReturnStatement",
                "argument": {
                  "type": "BinaryExpression",
                  "op": "+",
                  "left": { "type": "Identifier", "value": "a" },
                  "right": { "type": "Identifier", "value": "b" }
                }
              }
            ]
          }
        },
        {
          "type": "FunctionDeclaration",
          "name": { "type": "Identifier", "value": "noop" },
          "params": [],
          "body": {
            "type": "BlockStatement",
            "body": [ { "type": "ReturnStatement", "argument": null } ]
          }
        },
        {
          "type": "FunctionDeclaration",
          "name": { "type": "Identifier", "value": "empty" },
          "params": [],
          "body": { "type": "BlockStatement", "body": [] }
        },
        {
          "type": "ExpressionStatement",
          "expression": { "type": "BooleanLiteral", "value": true }
        },
        {
          "type": "ExpressionStatement",
          "expression": { "type": "BooleanLiteral", "value": false }
        },
        {
          "type": "ExpressionStatement",
          "expression": { "type": "NullLiteral", "value": null }
        },
        { "type": "EmptyStatement" }
      ]
    } |}
};
