%{
open Ast
%}

%token TWOVAR
%token THREEVAR
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
%token <string> STRING
%token <bool> BOOL
%token TIMES
%token PLUS
%token SUBT
%token DIV
%token EQ 
%token NEQ
%token LEQ
%token GEQ
%token LT
%token GT
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
%token IF 
%token THEN 
%token ELSE
%token GOTO 
%token LBL
%token EOF

%left PLUS
%left SUBT 
%left TIMES  
%left DIV
%left COMB 
%left PERM
%left POW
%left EQ
%left NEQ
%left LEQ
%left GEQ
%left LT
%left GT
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
	| s = STRING; { PreString s }
	| b = BOOL; { Val (Bool b) }
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
	| e1 = expr; EQ; e2 = expr { Binop (Eq, e1, e2) }
	| e1 = expr; NEQ; e2 = expr { Binop (Neq, e1, e2) }
	| e1 = expr; GEQ; e2 = expr { Binop (Geq, e1, e2) }
	| e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
	| e1 = expr; GT; e2 = expr { Binop (Gt, e1, e2) }
	| e1 = expr; LT; e2 = expr { Binop (Lt, e1, e2) }
	| SIN; e = expr; {Uniop (Sin, e)}
	| COS; e = expr; {Uniop (Cos, e)}
	| TAN; e = expr; {Uniop (Tan, e)}
	| ARCTAN; e = expr; {Uniop (ArcTan, e)}
	| ARCCOS; e = expr; {Uniop (ArcCos, e)}
	| ARCSIN; e = expr; {Uniop (ArcSin, e)}
	| VAR; s = NAME; COLON; e1 = expr; END; e2 = expr; { Bind (s, e1, e2) }
	| LPAREN; e=expr; RPAREN {e} 
	| SOLVE; THREEVAR; {Solver "three"}
	| SOLVE; TWOVAR; {Solver "two"}
	;
	
defn: 
	| s = NAME; COLON; e = expr; END; d = defn; { DAssign (s, e, d) }
	| s = NAME; COLON; e = expr; END; { DAssign (s, e, DEnd) }
	| s = NAME; COLON; e = expr; { DAssign (s, e, DEnd) }
	| DISP; e = expr; END; d = defn; { DDisp (e, d) }
	| DISP; e = expr; END; { DDisp (e, DEnd) }
	| DISP; e = expr; { DDisp (e, DEnd) }
	| IF; e = expr; END; THEN; d1 = short_defn; END; ELSE; d2 = short_defn; END; d3 = defn; { DIf (e, d1, d2, d3) }
	| IF; e = expr; END; THEN; END; d1 = short_defn; END; ELSE; END; d2 = short_defn; END; d3 = defn; { DIf (e, d1, d2, d3) }
	| IF; e = expr; THEN; d1 = short_defn; END; ELSE; d2 = short_defn; END; d3 = defn; { DIf (e, d1, d2, d3) }
	| IF; e = expr; THEN; END; d1 = short_defn; END; ELSE; END; d2 = short_defn; END; d3 = defn; { DIf (e, d1, d2, d3) }
	| IF; e = expr; THEN; END; d1 = short_defn; END; ELSE; END; d2 = short_defn; { DIf (e, d1, d2, DEnd) }
	| IF; e = expr; THEN; END; d1 = short_defn; END; ELSE; END; d2 = short_defn; END; { DIf (e, d1, d2, DEnd) }
	| IF; e = expr; THEN; d1 = short_defn; ELSE; d2 = short_defn; END; d3 = defn; { DIf (e, d1, d2, d3) }
	| IF; e = expr; THEN; d1 = short_defn; END; ELSE; d2 = short_defn; { DIf (e, d1, d2, DEnd) }
	| IF; e = expr; THEN; d1 = short_defn; END; ELSE; d2 = short_defn; END; { DIf (e, d1, d2, DEnd) }
	| IF; e = expr; THEN; d1 = short_defn; END; d2 = defn; { DIf (e, d1, DEnd, d2) }
	| IF; e = expr; THEN; d = short_defn; END; { DIf (e, d, DEnd, DEnd) }
	| IF; e = expr; THEN; d = short_defn; { DIf (e, d, DEnd, DEnd) }
	| GOTO; s = NAME; END; d = defn; { DGoto (s, d) }
	| LBL; s = NAME; END; d = defn; { DLabel (s, d) }
	| GOTO; s = NAME; END; { DGoto (s, DEnd) }
	| GOTO; s = NAME; { DGoto (s, DEnd) }
	| LBL; s = NAME; END; { DLabel (s, DEnd) }
	| LBL; s = NAME; { DLabel (s, DEnd) }
	| END; d = defn; { d }
	| d = defn; END; { d }
  | END; { DEnd }

short_defn:
	| DISP; e = expr; { DDisp (e, DEnd) }
	| s = NAME; COLON; e = expr; { DAssign (s, e, DEnd) }
	| GOTO; s = NAME; { DGoto (s, DEnd) }
	| LPAREN; d = defn; RPAREN; { d }
	| END; { DEnd }