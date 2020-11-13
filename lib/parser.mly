
/* Token definitions */

%token LPARENT
%token EOF

/* Specify starting production */
%start <string option> program 


%% /* Start grammar productions */

program: 
  | EOF       { None }
  | LPARENT { Some "cool" }