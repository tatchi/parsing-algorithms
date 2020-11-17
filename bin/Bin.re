print_endline("Hello!");

let parse = Lib.Parser.program(Lib.Lexer.read_token);

// switch (parse(Lexing.from_string("4+3*2"))) {
// | None => print_endline("Empty string")
// | Some(res) => print_endline(string_of_int(res))
// };

let res = parse(Lexing.from_string("(2+3)*3"));

// let rec toString = node => {
//   Lib.Parsed_Ast.(
//     switch (node) {
//     | NumericLiteral(n) => "NumericLiteral(" ++ string_of_int(n) ++ ")"
//     | BinaryExpression({left, op, right}) =>
//       toString(left) ++ op ++ toString(right)
//     }
//   );
// };

let json = Lib.Parsed_Ast.toString(res);

print_endline(json);