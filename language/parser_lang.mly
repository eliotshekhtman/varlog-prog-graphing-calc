%{
open Ast
%}

%token <float> NUM
%token TIMES
%token PLUS
%token SUBT
%token DIV
%token POW
%token EXCL
%token LPAREN
%token RPAREN
%token VAR
%token END
%token EOF

%left PLUS
%left SUBT 
%left TIMES  
%left DIV
%left POW

%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e }
	;
	
expr:
	| i = NUM { Num i }
	| s = NAME { Var s }
	| LPAREN; SUBT; e = expr; RPAREN { Binop (Subt, Num 0., e) }
	| e = expr; EXCL {Uniop (Fact, e)}
	| e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) } 
	| e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
	| e1 = expr; SUBT; e2 = expr { Binop (Subt, e1, e2) }
	| e1 = expr; DIV; e2 = expr { Binop (Div, e1, e2) }
	| e1 = expr; POW; e2 = expr { Binop (Pow, e1, e2) }
	| VAR; s = NAME; e1 = expr; END; e2 = expr; { Let (s, e1, e2) }
	| LPAREN; e=expr; RPAREN {e} 
	;
	
