
// %{
//   open Parsed_Ast
// %}

/* Token definitions */

// %token <string> ADDITIVE_OPERATOR
%token <int> NUMBER
%token PLUS
%token TIME
%token EOF


%left PLUS
%left TIME

/* Specify starting production */
%start program

%type <int> program


%% /* Start grammar productions */

program: 
  | e = E EOF { e }

E: 
  | n1 = E PLUS n2 = E { n1 + n2 }
  | n1 = E TIME n2 = E { n1 * n2 }
  | n = NUMBER { n }