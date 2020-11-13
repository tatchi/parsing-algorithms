print_endline("Hello!");

let parse = Lib.Parser.program(Lib.Lexer.read_token);

switch (parse(Lexing.from_string("("))) {
| None => print_endline("None")
| Some(res) => print_endline(res)
};
