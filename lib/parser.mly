
// %{
//   open Parsed_Ast
// %}

/* Token definitions */

// %token <string> ADDITIVE_OPERATOR
%token <int> NUMBER
%token PLUS
%token TIME
%token EOF

/* Specify starting production */
%start program

%type <int> program

%% /* Start grammar productions */

program: 
  | e = AdditiveExpression EOF { e }

AdditiveExpression: 
  | expr1 = AdditiveExpression PLUS expr2 = MultiplicativeExpression { expr1 + expr2 }
  | expr = MultiplicativeExpression { expr }
  ;

MultiplicativeExpression:
  | expr1 = MultiplicativeExpression TIME expr2 = PrimaryExpression { expr1 * expr2 }
  | expr = PrimaryExpression { expr }
  ;

PrimaryExpression:
  | n = NUMBER { n }
  ;