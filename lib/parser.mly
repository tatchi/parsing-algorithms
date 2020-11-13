
/* Token definitions */

%token <string> ADDITIVE_OPERATOR
%token <int> NUMBER
%token EOF

/* Specify starting production */
%start <int option> program 


%% /* Start grammar productions */

program: 
  | EOF       { None }
  | v = value  { Some v }
  ;

value: 
  | n1 = NUMBER; ADDITIVE_OPERATOR; n2 = NUMBER { n1+n2 }
  ;