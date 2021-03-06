
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
%token LBRACKET
%token RBRACKET
%token DOT
%token SEMICOLON
%token FUNCTION
%token <string> STRING
%token RETURN
%token TRUE
%token FALSE
%token EQUAL
%token LANGLE
%token RANGLE
%token OR
%token AND
%token LET
%token WHILE
%token FOR
%token NULL
%token CLASS
%token EXTENDS
%token SUPER
%token THIS
%token NEW
%token EQUALITY
%token DIFFERENCE
%token COMPLEX_ASSIGNMENT_MULT
%token COMPLEX_ASSIGNMENT_PLUS
%token COMPLEX_ASSIGNMENT_MINUS
%token IF
%token ELSE
%token <string> IDENTIFIER
%token COMMA
%token EOF

%nonassoc THEN
%nonassoc ELSE

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
  | IfStatement { $1 }
  | VariableStatement { $1 }
  | IterationStatement { $1 }
  | ClassDeclaration { ClassDeclaration($1) }
  ;

ClassDeclaration:
  | CLASS Identifier option(ClassExtends) Statement { {id=$2; superclass=$3;body=$4} }
  ;

ClassExtends: 
  | EXTENDS Identifier { $2 }
  ;

IterationStatement:
  | WhileStatement { IterationStatement($1) }
  | ForStatement { IterationStatement($1) }
  ;

WhileStatement:
  | WHILE LPAREN test=Expression RPAREN body=Statement { WhileStatement({test;body}) }
  ;

ForStatement:
  | FOR LPAREN init=Expression SEMICOLON test=Expression SEMICOLON update=Expression RPAREN body=Statement { ForStatement({init;test;update;body}) }
  ;

VariableStatement:
  | LET declarations=VariableDeclarationList { VariableStatement(declarations) }
  ;

VariableDeclarationList:
  | separated_nonempty_list(COMMA, VariableDeclaration) SEMICOLON { $1 }
  ;

VariableDeclaration:
  | id=Identifier init=option(VariableInitializer) { VariableDeclaration({id; init}) }
  ;

VariableInitializer:
  | EQUAL Expression { $2 }

IfStatement:
  | IF LPAREN exp=Expression RPAREN consequent=Statement { IfStatement(exp, consequent, None) } %prec THEN
  | IF LPAREN exp=Expression RPAREN consequent=Statement ELSE alternate=Statement { IfStatement(exp, consequent, Some(alternate)) }
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
  | AssignmentExpression { $1 }
  ;


AssignmentExpression: 
  | LogicalORExpression { $1 }
  | assignmentLeft=LeftHandSideExpression op=assignmentOp assignmentRight=AssignmentExpression { AssignmentExpression({assignmentLeft; assignmentOp=op; assignmentRight}) }
  ;

LeftHandSideExpression:
  | MemberExpression { LHandSideMemberExpression($1) }
  ;

/*
foor = 10
foo['bar'] = 10
foo[0] = 10
foo[0] = 10
foo.bar.baz = 10
*/
MemberExpression:
  | Identifier { MExpIdentifier($1) }
  | ThisExpression { MexpThisExpression }
  | MemberExpression DOT Identifier { MExpExpression({object_=$1; property=MExpPropertyIdentifier($3)}) }
  | MemberExpression LBRACKET Expression RBRACKET { MExpExpression({object_=$1; property=MExpPropertyExp($3)}) }
  ;

ThisExpression:
  | THIS { $1 }

LogicalORExpression:
  | LogicalANDExpression { $1 }
  | left=LogicalORExpression OR right=LogicalANDExpression { LogicalExpression({left; op=BinOpOr; right}) }
  ;

LogicalANDExpression:
  | EqualityExpression { $1 }
  | left=LogicalANDExpression AND right=EqualityExpression { LogicalExpression({left; op=BinOpAnd; right}) }
  ;

EqualityExpression:
  | RelationalExpression { $1 }
  | left=EqualityExpression op=equalityOp right=RelationalExpression { BinaryExpression({left; op; right}) }
  ;

RelationalExpression:
  | AdditiveExpression { $1 }
  | left=RelationalExpression op=relationalOp right=AdditiveExpression { BinaryExpression({left; op; right}) }
  ;

AdditiveExpression: 
  | left = AdditiveExpression op=additiveOp right = MultiplicativeExpression { BinaryExpression({left; op; right}) }
  | expr = MultiplicativeExpression { expr }
  ;

MultiplicativeExpression:
  | left = MultiplicativeExpression op=multiplicativeOp right = UnaryExpression { BinaryExpression({left; op; right})}
  | expr = UnaryExpression { expr }
  ;

UnaryExpression:
  | PrimaryExpression { $1 }
  | CallExpression { CallExpression($1) }
  | unaryOp=unaryOp argument=UnaryExpression {UnaryExpression({unaryOp; argument})}
  ;

CallExpression:
  | callee=Callee arguments=Arguments {RegularCallExp({callee; arguments})}
  | Super arguments=Arguments {SuperCallExp({arguments})}
  ;

Super:
  SUPER { $1 }

Callee:
  | LeftHandSideExpression { Callee_LeftHandSideExpression($1) }
  | CallExpression { Callee_CallExpression($1) }
  ;

Arguments:
  | LPAREN separated_list(COMMA, AssignmentExpression) RPAREN { ($2) }
  ;

PrimaryExpression:
  | Literal { Literal($1) }
  | NewExpression { NewExpression($1) }
  | LeftHandSideExpression { LeftHandSideExpression($1) }
  | ParenthesizedExpression { $1 }
  ;

NewExpression:
  | NEW MemberExpression Arguments { ({callee=$2;arguments=$3}) }
  ;

Literal:
  | NumericLiteral { $1 }
  | TrueLiteral { $1 }
  | FalseLiteral { $1 }
  | NullLiteral { $1 }
  | StringLiteral { $1 }
  ;

NumericLiteral:
  | NUMBER { NumericLiteral($1) }
  ;
TrueLiteral:
  | TRUE { BooleanLiteral(true) }
  ;
FalseLiteral:
  | FALSE { BooleanLiteral(false) }
  ;
NullLiteral:
  | NULL { NullLiteral }
  ;

StringLiteral:
  | STRING { StringLiteral($1) }

ParenthesizedExpression:
  | LPAREN expr = Expression RPAREN { expr }
  ;


/* Operator expressions */

%inline unaryOp:
| PLUS { UnaryOpPlus }
| MINUS { UnaryOpMinus }

%inline additiveOp:
| PLUS { BinOpPlus }
| MINUS { BinOpMinus }

%inline multiplicativeOp:
| MULT { BinOpMult }
| DIV { BinOpDiv }

%inline relationalOp:
| LANGLE { BinOpLessThan }
| LANGLE EQUAL { BinOpLessThanEq }
| RANGLE { BinOpGreaterThan }
| RANGLE EQUAL { BinOpGreaterThanEq }

%inline equalityOp:
| EQUALITY { BinOpEq }
| DIFFERENCE { BinOpNotEq }

%inline assignmentOp:
| EQUAL { AssignmentOpEq }
| COMPLEX_ASSIGNMENT_MULT { AssignmentOpEqMult }
| COMPLEX_ASSIGNMENT_PLUS { AssignmentOpEqPlus }
| COMPLEX_ASSIGNMENT_MINUS { AssignmentOpEqMinus }