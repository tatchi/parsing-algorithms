
%{
  open Parsed_Ast
%}

/* Token definitions */

%token <string> ADDITIVE_OPERATOR
%token <int> NUMBER
%token EOF

/* Specify starting production */
%start program 

%type <Parsed_Ast.t option> program


%% /* Start grammar productions */

program: 
  | EOF       { None }
  | v = value  { Some v }
  ;

value: 
  | left = NUMBER; op = ADDITIVE_OPERATOR; right = NUMBER { {op; left; right} }
  ;