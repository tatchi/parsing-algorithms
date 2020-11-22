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
        { "type": "EmptyStatement" }
      ]
    } |}
};
