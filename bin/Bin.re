print_endline("Hello!");

let parse = Lib.Parser.program(Lib.Lexer.read_token);

// switch (parse(Lexing.from_string("4+3*2"))) {
// | None => print_endline("Empty string")
// | Some(res) => print_endline(string_of_int(res))
// };

let res = (parse(Lexing.from_string("4*3+2")));

print_endline(string_of_int(res));
