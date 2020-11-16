
// %{
//   open Parsed_Ast
// %}

/* Token definitions */

// %token <string> ADDITIVE_OPERATOR
%token <int> NUMBER
%token PLUS
%token TIME
%token LPAREN
%token RPAREN
%token EOF

/* Specify starting production */
%start program

%type <int> program

%% /* Start grammar productions */

program: 
  | e = Expression EOF { e }

Expression:
  | expr = AdditiveExpression { expr }
  ;

AdditiveExpression: 
  | expr1 = AdditiveExpression PLUS expr2 = MultiplicativeExpression { expr1 + expr2 }
  | expr = MultiplicativeExpression { expr }
  ;

MultiplicativeExpression:
  | expr1 = MultiplicativeExpression TIME expr2 = PrimaryExpression { expr1 * expr2 }
  | expr = PrimaryExpression { expr }
  ;

PrimaryExpression:
  | lt = Literal { lt }
  | p = ParenthesizedExpression { p }
  ;

Literal:
  | n = NumericLiteral { n }
  ;

NumericLiteral:
  | n = NUMBER { n }
  ;

ParenthesizedExpression:
  | LPAREN expr = Expression RPAREN { expr }
  ;
