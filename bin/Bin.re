print_endline("Hello!");

open Lexing;

let print_error_position = lexbuf => {
  let pos = lexbuf.lex_curr_p;
  Fmt.str(
    "Line:%d Position:%d",
    pos.pos_lnum,
    pos.pos_cnum - pos.pos_bol + 1,
  );
};
let parse_program = lexbuf =>
  try(Lib.Parser.program(Lib.Lexer.read_token, lexbuf)) {
  | Lib.Lexer.SyntaxError(msg) =>
    let error_msg = Fmt.str("%s: %s@.", print_error_position(lexbuf), msg);
    print_endline(error_msg);
    exit(-1);
  | Lib.Parser.Error =>
    let error_msg =
      Fmt.str(
        "%s: syntax error. Token: %s",
        print_error_position(lexbuf),
        Lexing.lexeme(lexbuf),
      );
    print_endline(error_msg);
    exit(-1);
  };

let res = parse_program(Lexing.from_string("(2+3)*3; 4+1;"));

let json = Lib.Parsed_Ast.toString(res);

print_endline(json);
