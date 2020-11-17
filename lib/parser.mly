
%{
  open Parsed_Ast
%}

/* Token definitions */

%token <int> NUMBER
%token PLUS
%token MINUS
%token MULT
%token DIV
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
  | left = AdditiveExpression op=additiveOp right = MultiplicativeExpression { BinaryExpression({left; op; right}) }
  | expr = MultiplicativeExpression { expr }
  ;

MultiplicativeExpression:
  | left = MultiplicativeExpression op=multiplicativeOp right = PrimaryExpression { BinaryExpression({left; op; right})}
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


/* Operator expressions */

%inline additiveOp:
| PLUS { BinOpPlus }
| MINUS { BinOpMinus }

%inline multiplicativeOp:
| MULT { BinOpMult }
| DIV { BinOpDiv }