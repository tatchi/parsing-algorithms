let%expect_test "parse program" = {
  print_endline(Sys.getcwd())
  let parsed =
    Parser.program(Lexer.read_token, Lexing.from_channel(Stdlib.open_in("./data.txt")));
  let json = Parsed_Ast.toString(parsed);
  print_endline(json);
  %expect
  {|
    /Users/corentin/Dev/parsing-algorithms/_build/default/lib
    {
      "type": "program",
      "body": [
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "BinaryExpression",
            "operator": "+",
            "left": { "type": "NumericLiteral", "value": 2 },
            "right": { "type": "NumericLiteral", "value": 2 }
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "BinaryExpression",
            "operator": "+",
            "left": { "type": "NumericLiteral", "value": 5 },
            "right": {
              "type": "BinaryExpression",
              "operator": "*",
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
                "operator": "+",
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
                    "operator": "+",
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
                  "operator": "+",
                  "left": { "type": "NumericLiteral", "value": 1 },
                  "right": { "type": "NumericLiteral", "value": 2 }
                }
              },
              {
                "type": "ReturnStatement",
                "argument": {
                  "type": "BinaryExpression",
                  "operator": "+",
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
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "BinaryExpression",
            "operator": ">",
            "left": { "type": "NumericLiteral", "value": 5 },
            "right": { "type": "NumericLiteral", "value": 3 }
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "BinaryExpression",
            "operator": ">=",
            "left": { "type": "NumericLiteral", "value": 5 },
            "right": { "type": "NumericLiteral", "value": 3 }
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "BinaryExpression",
            "operator": "<=",
            "left": { "type": "NumericLiteral", "value": 5 },
            "right": { "type": "NumericLiteral", "value": 3 }
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "BinaryExpression",
            "operator": "<",
            "left": { "type": "NumericLiteral", "value": 5 },
            "right": { "type": "NumericLiteral", "value": 3 }
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "BinaryExpression",
            "operator": "==",
            "left": { "type": "NumericLiteral", "value": 5 },
            "right": { "type": "NumericLiteral", "value": 3 }
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "BinaryExpression",
            "operator": "!=",
            "left": { "type": "NumericLiteral", "value": 5 },
            "right": { "type": "NumericLiteral", "value": 3 }
          }
        },
        {
          "type": "IfStatement",
          "test": { "type": "BooleanLiteral", "value": true },
          "consequent": {
            "type": "IfStatement",
            "test": { "type": "BooleanLiteral", "value": false },
            "consequent": {
              "type": "ReturnStatement",
              "argument": { "type": "NumericLiteral", "value": 100 }
            },
            "alternate": {
              "type": "ReturnStatement",
              "argument": { "type": "NumericLiteral", "value": 200 }
            }
          },
          "alternate": null
        },
        {
          "type": "IfStatement",
          "test": {
            "type": "LogicalExpression",
            "operator": "||",
            "left": {
              "type": "LogicalExpression",
              "operator": "&&",
              "left": {
                "type": "BinaryExpression",
                "operator": "==",
                "left": { "type": "Identifier", "value": "a" },
                "right": { "type": "NumericLiteral", "value": 10 }
              },
              "right": {
                "type": "BinaryExpression",
                "operator": "<",
                "left": { "type": "Identifier", "value": "b" },
                "right": { "type": "NumericLiteral", "value": 10 }
              }
            },
            "right": {
              "type": "BinaryExpression",
              "operator": "==",
              "left": { "type": "Identifier", "value": "b" },
              "right": { "type": "NumericLiteral", "value": 2 }
            }
          },
          "consequent": { "type": "BlockStatement", "body": [] },
          "alternate": null
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "AssignmentExpression",
            "operator": "=",
            "left": { "type": "Identifier", "value": "a" },
            "right": {
              "type": "AssignmentExpression",
              "operator": "=",
              "left": { "type": "Identifier", "value": "b" },
              "right": { "type": "NumericLiteral", "value": 5 }
            }
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "AssignmentExpression",
            "operator": "*=",
            "left": { "type": "Identifier", "value": "a" },
            "right": {
              "type": "AssignmentExpression",
              "operator": "-=",
              "left": { "type": "Identifier", "value": "b" },
              "right": {
                "type": "AssignmentExpression",
                "operator": "+=",
                "left": { "type": "Identifier", "value": "c" },
                "right": {
                  "type": "AssignmentExpression",
                  "operator": "=",
                  "left": { "type": "Identifier", "value": "d" },
                  "right": { "type": "NumericLiteral", "value": 9 }
                }
              }
            }
          }
        },
        {
          "type": "VariableStatement",
          "declarations": [
            {
              "type": "VariableDeclaration",
              "id": { "type": "Identifier", "value": "a" },
              "init": { "type": "NumericLiteral", "value": 5 }
            }
          ]
        },
        {
          "type": "VariableStatement",
          "declarations": [
            {
              "type": "VariableDeclaration",
              "id": { "type": "Identifier", "value": "a" },
              "init": {
                "type": "AssignmentExpression",
                "operator": "=",
                "left": { "type": "Identifier", "value": "b" },
                "right": { "type": "NumericLiteral", "value": 6 }
              }
            }
          ]
        },
        {
          "type": "VariableStatement",
          "declarations": [
            {
              "type": "VariableDeclaration",
              "id": { "type": "Identifier", "value": "a" },
              "init": null
            },
            {
              "type": "VariableDeclaration",
              "id": { "type": "Identifier", "value": "c" },
              "init": { "type": "NumericLiteral", "value": 7 }
            }
          ]
        },
        {
          "type": "VariableStatement",
          "declarations": [
            {
              "type": "VariableDeclaration",
              "id": { "type": "Identifier", "value": "a" },
              "init": { "type": "NumericLiteral", "value": 6 }
            },
            {
              "type": "VariableDeclaration",
              "id": { "type": "Identifier", "value": "c" },
              "init": null
            },
            {
              "type": "VariableDeclaration",
              "id": { "type": "Identifier", "value": "d" },
              "init": {
                "type": "LogicalExpression",
                "operator": "||",
                "left": { "type": "BooleanLiteral", "value": true },
                "right": { "type": "NumericLiteral", "value": 5 }
              }
            }
          ]
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "UnaryExpression",
            "operator": "-",
            "argument": { "type": "NumericLiteral", "value": 10 }
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "UnaryExpression",
            "operator": "-",
            "argument": { "type": "Identifier", "value": "x" }
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "UnaryExpression",
            "operator": "+",
            "argument": { "type": "NumericLiteral", "value": 20 }
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "BinaryExpression",
            "operator": "+",
            "left": {
              "type": "UnaryExpression",
              "operator": "-",
              "argument": { "type": "NumericLiteral", "value": 10 }
            },
            "right": {
              "type": "UnaryExpression",
              "operator": "-",
              "argument": { "type": "Identifier", "value": "x" }
            }
          }
        },
        {
          "type": "FunctionDeclaration",
          "name": { "type": "Identifier", "value": "getMagic" },
          "params": [],
          "body": {
            "type": "BlockStatement",
            "body": [
              {
                "type": "ReturnStatement",
                "argument": {
                  "type": "UnaryExpression",
                  "operator": "-",
                  "argument": { "type": "NumericLiteral", "value": 10 }
                }
              }
            ]
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "UnaryExpression",
            "operator": "-",
            "argument": {
              "type": "CallExpression",
              "callee": { "type": "Identifier", "value": "getMagic" },
              "arguments": []
            }
          }
        },
        {
          "type": "FunctionDeclaration",
          "name": { "type": "Identifier", "value": "getCallback" },
          "params": [],
          "body": {
            "type": "BlockStatement",
            "body": [
              {
                "type": "FunctionDeclaration",
                "name": { "type": "Identifier", "value": "inner" },
                "params": [],
                "body": {
                  "type": "BlockStatement",
                  "body": [
                    {
                      "type": "ReturnStatement",
                      "argument": { "type": "NumericLiteral", "value": 10 }
                    }
                  ]
                }
              },
              {
                "type": "ReturnStatement",
                "argument": { "type": "Identifier", "value": "inner" }
              }
            ]
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "UnaryExpression",
            "operator": "-",
            "argument": {
              "type": "CallExpression",
              "callee": {
                "type": "CallExpression",
                "callee": { "type": "Identifier", "value": "getCallback" },
                "arguments": []
              },
              "arguments": []
            }
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "BinaryExpression",
            "operator": "*",
            "left": { "type": "NumericLiteral", "value": 10 },
            "right": {
              "type": "UnaryExpression",
              "operator": "-",
              "argument": {
                "type": "CallExpression",
                "callee": { "type": "Identifier", "value": "getMagic" },
                "arguments": []
              }
            }
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": { "type": "StringLiteral", "value": "hello world" }
        },
        {
          "type": "VariableStatement",
          "declarations": [
            {
              "type": "VariableDeclaration",
              "id": { "type": "Identifier", "value": "s" },
              "init": { "type": "StringLiteral", "value": "my super string" }
            }
          ]
        },
        {
          "type": "WhileStatement",
          "test": {
            "type": "BinaryExpression",
            "operator": ">",
            "left": { "type": "Identifier", "value": "i" },
            "right": { "type": "NumericLiteral", "value": 0 }
          },
          "body": {
            "type": "BlockStatement",
            "body": [
              {
                "type": "ExpressionStatement",
                "expression": {
                  "type": "CallExpression",
                  "callee": {
                    "type": "MemberExpression",
                    "object": { "type": "Identifier", "value": "console" },
                    "property": { "type": "Identifier", "value": "log" }
                  },
                  "arguments": [
                    { "type": "Identifier", "value": "i" },
                    {
                      "type": "MemberExpression",
                      "object": { "type": "Identifier", "value": "s" },
                      "property": { "type": "Identifier", "value": "i" }
                    }
                  ]
                }
              },
              {
                "type": "ExpressionStatement",
                "expression": {
                  "type": "AssignmentExpression",
                  "operator": "=",
                  "left": { "type": "Identifier", "value": "i" },
                  "right": {
                    "type": "BinaryExpression",
                    "operator": "-",
                    "left": { "type": "Identifier", "value": "i" },
                    "right": { "type": "NumericLiteral", "value": 1 }
                  }
                }
              }
            ]
          }
        },
        {
          "type": "ForStatement",
          "init": {
            "type": "AssignmentExpression",
            "operator": "=",
            "left": { "type": "Identifier", "value": "i" },
            "right": { "type": "NumericLiteral", "value": 0 }
          },
          "test": {
            "type": "BinaryExpression",
            "operator": "<",
            "left": { "type": "Identifier", "value": "i" },
            "right": { "type": "NumericLiteral", "value": 5 }
          },
          "update": {
            "type": "BinaryExpression",
            "operator": "<",
            "left": { "type": "Identifier", "value": "i" },
            "right": { "type": "NumericLiteral", "value": 5 }
          },
          "body": {
            "type": "BlockStatement",
            "body": [
              {
                "type": "ExpressionStatement",
                "expression": {
                  "type": "AssignmentExpression",
                  "operator": "=",
                  "left": { "type": "Identifier", "value": "i" },
                  "right": {
                    "type": "BinaryExpression",
                    "operator": "-",
                    "left": { "type": "Identifier", "value": "i" },
                    "right": { "type": "NumericLiteral", "value": 1 }
                  }
                }
              }
            ]
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "AssignmentExpression",
            "operator": "=",
            "left": {
              "type": "MemberExpression",
              "object": { "type": "Identifier", "value": "foo" },
              "property": { "type": "Identifier", "value": "bar" }
            },
            "right": { "type": "NumericLiteral", "value": 10 }
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "AssignmentExpression",
            "operator": "=",
            "left": {
              "type": "MemberExpression",
              "object": { "type": "Identifier", "value": "s" },
              "property": { "type": "Identifier", "value": "i" }
            },
            "right": { "type": "NumericLiteral", "value": 20 }
          }
        },
        {
          "type": "ExpressionStatement",
          "expression": {
            "type": "MemberExpression",
            "object": {
              "type": "MemberExpression",
              "object": { "type": "Identifier", "value": "foo" },
              "property": { "type": "Identifier", "value": "bar" }
            },
            "property": { "type": "Identifier", "value": "baz" }
          }
        },
        {
          "type": "ClassDeclaration",
          "id": { "type": "Identifier", "value": "Point" },
          "superclass": null,
          "body": {
            "type": "BlockStatement",
            "body": [
              {
                "type": "ExpressionStatement",
                "expression": { "type": "ThisExpression" }
              },
              {
                "type": "ExpressionStatement",
                "expression": {
                  "type": "AssignmentExpression",
                  "operator": "=",
                  "left": {
                    "type": "MemberExpression",
                    "object": { "type": "ThisExpression" },
                    "property": { "type": "Identifier", "value": "a" }
                  },
                  "right": { "type": "NumericLiteral", "value": 5 }
                }
              },
              {
                "type": "ExpressionStatement",
                "expression": {
                  "type": "AssignmentExpression",
                  "operator": "=",
                  "left": {
                    "type": "MemberExpression",
                    "object": { "type": "ThisExpression" },
                    "property": {
                      "type": "MemberExpression",
                      "object": { "type": "ThisExpression" },
                      "property": { "type": "Identifier", "value": "a" }
                    }
                  },
                  "right": { "type": "NumericLiteral", "value": 7 }
                }
              }
            ]
          }
        },
        {
          "type": "ClassDeclaration",
          "id": { "type": "Identifier", "value": "Point3D" },
          "superclass": { "type": "Identifier", "value": "Point" },
          "body": {
            "type": "BlockStatement",
            "body": [
              {
                "type": "ExpressionStatement",
                "expression": {
                  "type": "CallExpression",
                  "callee": "Super",
                  "arguments": [ { "type": "NumericLiteral", "value": 5 } ]
                }
              },
              {
                "type": "FunctionDeclaration",
                "name": { "type": "Identifier", "value": "getX" },
                "params": [],
                "body": {
                  "type": "BlockStatement",
                  "body": [
                    {
                      "type": "ReturnStatement",
                      "argument": {
                        "type": "MemberExpression",
                        "object": { "type": "ThisExpression" },
                        "property": { "type": "Identifier", "value": "x" }
                      }
                    }
                  ]
                }
              }
            ]
          }
        },
        {
          "type": "VariableStatement",
          "declarations": [
            {
              "type": "VariableDeclaration",
              "id": { "type": "Identifier", "value": "a" },
              "init": {
                "type": "NewExpression",
                "callee": { "type": "Identifier", "value": "Point" },
                "arguments": [
                  { "type": "NumericLiteral", "value": 10 },
                  { "type": "NumericLiteral", "value": 20 }
                ]
              }
            }
          ]
        },
        { "type": "EmptyStatement" }
      ]
    } |}
};
