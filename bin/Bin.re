print_endline("Hello!");

let parse = Lib.Parser.program(Lib.Lexer.read_token);

switch (parse(Lexing.from_string("4-  3  "))) {
| None => print_endline("Empty string")
| Some({op, left, right}) =>
  print_endline(
    string_of_int(left) ++ op ++ string_of_int(right),
  )
};
