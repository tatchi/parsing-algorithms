
%{
  open Parsed_Ast
%}

/* Token definitions */

%token <int> NUMBER
%token <string> ADDITIVE_OPERATOR
%token <string> MULTIPLICATIVE_OPERATOR
%token LPAREN
%token RPAREN
%token EOF

/* Specify starting production */
%start program

%type <Parsed_Ast.t> program

%% /* Start grammar productions */

program: 
  | e = Expression EOF { e }

Expression:
  | expr = AdditiveExpression { expr }
  ;

AdditiveExpression: 
  | left = AdditiveExpression op = ADDITIVE_OPERATOR right = MultiplicativeExpression { BinaryExpression({left; op; right}) }
  | expr = MultiplicativeExpression { expr }
  ;

MultiplicativeExpression:
  | left = MultiplicativeExpression op = MULTIPLICATIVE_OPERATOR right = PrimaryExpression { BinaryExpression({left; op; right})}
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
  | n = NUMBER { NumericLiteral(n) }
  ;

ParenthesizedExpression:
  | LPAREN expr = Expression RPAREN { expr }
  ;
