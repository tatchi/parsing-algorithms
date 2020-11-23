
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
%token LBRACE
%token RBRACE
%token SEMICOLON
%token FUNCTION
%token RETURN
%token <string> IDENTIFIER
%token COMMA
%token EOF

/* Specify starting production */
%start program

%type <Parsed_Ast.t> program

%% /* Start grammar productions */

program: 
  | StatementList EOF { Program($1) }

StatementList:
  | statements = list(Statement) { statements }
  ;

Statement:
  | ExpressionStatement { ExpressionStatement($1) }
  | EmptyStatement { EmptyStatement }
  | BlockStatement { BlockStatement($1) }
  | FunctionDeclaration { $1 }
  | ReturnStatement { $1 }
  ;

FunctionDeclaration:
  | FUNCTION name=Identifier LPAREN params=Params RPAREN statement=BlockStatement { FunctionDeclaration(name, params, statement) }
  ;

Params:
  | separated_list(COMMA,Identifier) { $1 }
  ;

Identifier:
  | IDENTIFIER { $1 }
  ;

ReturnStatement:
  | RETURN expr = option(Expression) SEMICOLON { ReturnStatement(expr) }
  ;

ExpressionStatement:
  | Expression SEMICOLON { Expression($1) }
  ;

EmptyStatement:
  | SEMICOLON { ($1) }
  ;

BlockStatement:
  | LBRACE statements=StatementList RBRACE { statements }
  ;

Expression:
  | AdditiveExpression { $1 }
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
  | Literal { $1 }
  | Identifier { Identifier($1) }
  | ParenthesizedExpression { $1 }
  ;

Literal:
  | NumericLiteral { $1 }
  ;

NumericLiteral:
  | NUMBER { NumericLiteral($1) }
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