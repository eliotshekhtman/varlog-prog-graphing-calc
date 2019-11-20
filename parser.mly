%{
open Ast
%}

%token SOLVE
%token EVAL
%token GRAPH
%token NEWTON
%token EXEC
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
%token QUOTE
%token DISP
%token END
%token IF 
%token THEN 
%token ELSE
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

%start <Ast.expr> parse_expr 
%start <Ast.defn> parse_defn

%%

parse_expr:
	| e = expr; EOF { e }
	;

parse_defn: 
  | d = defn; EOF { d }
	;

expr:
  | EVAL; e = expr; { Keyword (Eval, e) }
	| GRAPH; e = expr; { Keyword (Graph, e) }
	| NEWTON; e = expr; { Keyword (Newton, e) }
	| EXEC; e = expr; { Keyword (Exec, e) }
	| INTEGRAL; LPAREN; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; COMMA; e3 = expr RPAREN;
		{ Ternop (Integral, (e1 , e2), e3) }
	| DERIVATIVE; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN;{Derivative (Der, e1, e2)}
	| i = NUM { Val (Num i) }
	| QUOTE; s = NAME; QUOTE; { Val (Str s) }
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
	| SOLVE; {Solver}
	;
	
defn: 
  | VAR; s = NAME; COLON; e = expr; END; d = defn; { DBind (s, e, d) }
	| VAR; s = NAME; COLON; e = expr; END; { DBind (s, e, DEnd) }
	| VAR; s = NAME; COLON; e = expr; { DBind (s, e, DEnd) }
	| s = NAME; COLON; e = expr; END; d = defn; { DAssign (s, e, d) }
	| s = NAME; COLON; e = expr; END; { DAssign (s, e, DEnd) }
	| s = NAME; COLON; e = expr; { DAssign (s, e, DEnd) }
	| DISP; e = expr; END; d = defn; { DDisp (e, d) }
	| DISP; e = expr; END; { DDisp (e, DEnd) }
	| DISP; e = expr; { DDisp (e, DEnd) }
	| IF; e = expr; THEN; d1 = defn; END; ELSE; d2 = defn; END; d3 = defn; { DIf (e, d1, d2, d3) }
	| IF; e = expr; THEN; d1 = defn; END; d2 = defn; { DIf (e, d1, DEnd, d2) }
	| IF; e = expr; THEN; d = defn; END; { DIf (e, d, DEnd, DEnd) }
	| IF; e = expr; THEN; d = defn; { DIf (e, d, DEnd, DEnd) }
	| END; d = defn; { d }
	| d = defn; END; { d }
  | END; { DEnd }