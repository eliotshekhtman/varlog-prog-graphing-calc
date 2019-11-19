%{
open Ast
%}

%token EVAL
%token GRAPH
%token NEWTON
%token INTEGRAL
%token DERIVATIVE
%token SIN
%token COS
%token TAN
%token ARCTAN
%token ARCSIN
%token ARCCOS
%token <float> NUM
%token <string> NAME
%token TIMES
%token PLUS
%token SUBT
%token DIV
%token POW
%token EXCL
%token COMB 
%token PERM
%token LPAREN
%token RPAREN
%token COMMA
%token VAR
%token COLON
%token DISP
%token END
%token EOF

%left PLUS
%left SUBT 
%left TIMES  
%left DIV
%left COMB 
%left PERM
%left POW
%right SIN 
%right COS 
%right TAN
%right ARCSIN
%right ARCCOS
%right ARCTAN

%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e }
	;
	
expr:
  | EVAL; e = expr; { Keyword (Eval, e) }
	| GRAPH; e = expr; { Keyword (Graph, e) }
	| NEWTON; e = expr; { Keyword (Newton, e) }
	| INTEGRAL; LPAREN; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; COMMA; e3 = expr RPAREN;
		{ Ternop (Integral, (e1 , e2), e3) }
	| DERIVATIVE; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN;{Derivative (Der, e1, e2)}
	| i = NUM { Val (Num i) }
	| s = NAME { Var s }
	| LPAREN; SUBT; e = expr; RPAREN { Binop (Subt, Val (Num 0.), e) }
	| e = expr; EXCL {Uniop (Fact, e)}
	| SUBT; e = expr; { Uniop (Subt, e) }
	| e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) } 
	| e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
	| e1 = expr; SUBT; e2 = expr { Binop (Subt, e1, e2) }
	| e1 = expr; DIV; e2 = expr { Binop (Div, e1, e2) }
	| e1 = expr; POW; e2 = expr { Binop (Pow, e1, e2) }
	| e1 = expr; COMB; e2 = expr { Binop (Comb, e1, e2) }
	| e1 = expr; PERM; e2 = expr { Binop (Perm, e1, e2) }
	| SIN; e = expr; {Uniop (Sin, e)}
	| COS; e = expr; {Uniop (Cos, e)}
	| TAN; e = expr; {Uniop (Tan, e)}
	| ARCTAN; e = expr; {Uniop (ArcTan, e)}
	| ARCCOS; e = expr; {Uniop (ArcCos, e)}
	| ARCSIN; e = expr; {Uniop (ArcSin, e)}
	| DISP; e1 = expr; END; e2 = expr { Disp (e1, e2) }
	| VAR; s = NAME; COLON; e1 = expr; END; e2 = expr; { Bind (s, e1, e2) }
	| LPAREN; e=expr; RPAREN {e} 
	;
	
