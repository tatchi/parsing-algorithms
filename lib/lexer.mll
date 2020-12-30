{
open Lexing
open Parser
exception SyntaxError of string
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = digit+  (* regex for integers *)
let id = (alpha) (alpha|digit|'_')* (* regex for identifier *)
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read_token =
  parse
  | whitespace    { read_token lexbuf }
  | newline { next_line lexbuf; read_token lexbuf }
  | int { NUMBER (int_of_string (Lexing.lexeme lexbuf))}
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULT }
  | '/' { DIV }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | ";" { SEMICOLON }
  | "," { COMMA }
  | "let" { LET }
  | "=" { EQUAL }
  | "<" { LANGLE }
  | ">" { RANGLE }
  | "||" { OR }
  | "&&" { AND }
  | "==" { EQUALITY }
  | "!=" { DIFFERENCE }
  | "*=" { COMPLEX_ASSIGNMENT_MULT }
  | "+=" { COMPLEX_ASSIGNMENT_PLUS }
  | "-=" { COMPLEX_ASSIGNMENT_MINUS }
  | "true" { TRUE }
  | "false" { FALSE }
  | "null" { NULL }
  | "if" { IF }
  | "else" { ELSE }
  | "return" { RETURN }
  | "function" { FUNCTION }
  | id { IDENTIFIER (Lexing.lexeme lexbuf) }
  | "//" { read_single_line_comment lexbuf }
  | "/*" { read_multi_line_comment lexbuf } 
  | eof { EOF }
  | _ {raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }

and read_single_line_comment = parse
  | newline { next_line lexbuf; read_token lexbuf } 
  | eof { EOF }
  | _ { read_single_line_comment lexbuf } 

and read_multi_line_comment = parse
  | "*/" { read_token lexbuf } 
  | newline { next_line lexbuf; read_multi_line_comment lexbuf } 
  | eof { raise (SyntaxError ("Lexer - Unexpected EOF - please terminate your comment.")) }
  | _ { read_multi_line_comment lexbuf } 


