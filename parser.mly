%{
open Ast
let has_dups lst =
  let open List in
  length lst <> length (sort_uniq Stdlib.compare lst)

%}
%token MATRIX
%token VARMAT
%token RARROW 
%token RBRACKET
%token LBRACKET
%token ARROW
%token TWOVAR
%token THREEVAR
%token SOLVE
%token GRAPH
%token NEWTON
%token EXEC
%token SETSCALE
%token INTEGRAL
%token DERIVATIVE
%token RANDINT
%token LINE
%token RED 
%token GREEN 
%token BLUE 
%token BLACK 
%token YELLOW
%token SIN
%token COS
%token TAN
%token ARCTAN
%token ARCSIN
%token ARCCOS
%token NOT
%token <float> NUM
%token <string> NAME
%token <string> STRING
%token <bool> BOOL
/* %token <string> ID STRING */
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
%token AND 
%token OR 
%token NAND 
%token NOR
%token XOR 
%token NXOR
%token POW
%token EXCL
%token COMB 
%token PERM
%token LPAREN
%token RPAREN
%token LCURLY
%token RCURLY
%token COMMA
%token DOT
%token VAR
%token COLON
%token QUESTION
%token DOLLAR
%token DISP
%token END
%token IF 
%token THEN 
%token ELSE
%token GOTO 
%token GOTOSUB
%token WHILE
%token RETURN
%token LBL
%token GETKEY
%token PROMPT
%token OUTPUT
%token STRUCT
%token CLASS
%token EOF
%token FUN

%left AND 
%left OR
%left NAND 
%left NOR
%left XOR 
%left NXOR
%left EQ
%left NEQ
%left LEQ
%left GEQ
%left LT
%left GT
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

%start <Ast.phrase> parse_phrase
%start <Ast.expr> parse_expr 
%start <Ast.defn> parse_defn

%%

parse_phrase:
	| GRAPH; e = expr; EOF { Graph e }
	| NEWTON; e = expr; EOF { Newton e }
	| EXEC; e = expr; EOF { Exec e }
	| SETSCALE; EOF { SetScale }
	| SOLVE; THREEVAR; EOF {Solver "three"}
	| SOLVE; TWOVAR; EOF {Solver "two"}
  | e = expr; EOF { Expr e }
	| d = defn; EOF { Defn d }
	;

parse_expr:
	| e = expr; EOF { e }
	;

parse_defn: 
  | d = defn; EOF { d }
	;

ident:
  | x = NAME
        { x }

expr:
	| INTEGRAL; LPAREN; LPAREN; e1 = expr; COMMA; 
	  e2 = expr; RPAREN; COMMA; e3 = expr RPAREN;
		{ Ternop (Integral, (e1 , e2), e3) }
	| DERIVATIVE; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; 
	  { Derivative (Der, e1, e2) }
	| m = expr; LBRACKET; e1 = expr; COMMA; e2 = expr; RBRACKET; 
	  { MatrixGet (m, e1, e2) }
	| RANDINT; LPAREN e1 = expr; COMMA; e2 = expr; RPAREN; { RandInt (e1, e2) }
	| i = NUM { Val (Num i) }
	| s = STRING; { PreString s }
	| b = BOOL; { Val (Bool b) }
	| RED { Val (Color Red)}
	| YELLOW { Val (Color Yellow)}
	| BLUE { Val (Color Blue)}
	| BLACK { Val (Color Black)}
	| GREEN { Val (Color Green)}
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
	| e1 = expr; AND; e2 = expr { Boolop (And, e1, e2) }
	| e1 = expr; NOR; e2 = expr { Uniop (Not, Boolop (Or, e1, e2)) }
	| e1 = expr; NAND; e2 = expr { Uniop (Not, Boolop (And, e1, e2)) }
	| e1 = expr; OR; e2 = expr { Boolop (Or, e1, e2) }
	| e1 = expr; XOR; e2 = expr { Boolop (Xor, e1, e2) }
	| e1 = expr; NXOR; e2 = expr { Uniop (Not, Boolop (Xor, e1, e2)) }
	| g = expr; QUESTION; e1 = expr; COLON; e2 = expr; { Ternary (g, e1, e2) }
	| NOT; e = expr; { Uniop (Not, e) }
	| SIN; e = expr; {Uniop (Sin, e)}
	| COS; e = expr; {Uniop (Cos, e)}
	| TAN; e = expr; {Uniop (Tan, e)}
	| ARCTAN; e = expr; {Uniop (ArcTan, e)}
	| ARCCOS; e = expr; {Uniop (ArcCos, e)}
	| ARCSIN; e = expr; {Uniop (ArcSin, e)}
	| GETKEY; { GetKey }
	| PROMPT; { Prompt }
	| MATRIX; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; { MakeMatrix (e1, e2) }
	| VARMAT; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; { MakeVarMat (e1, e2) }
	| n = NAME; DOLLAR; s = NAME; { StructGet (n, s) }
	| n = NAME; DOT; s = NAME; { ObjectGet (n, s) }
	| n = NAME; RARROW; LPAREN; es = nonempty_list(expr); RPAREN; 
	  { Application(n,es) }
	| n = NAME; RARROW; LPAREN; RPAREN; { Application(n,[]) }
	| e = expr; RARROW; LPAREN; es = nonempty_list(expr); RPAREN; { ComplApp (e, es) }
	| e = expr; RARROW; LPAREN; RPAREN; { ComplApp (e, []) }
	| LPAREN; e=expr; RPAREN {e} 

	
defn: 
  | GRAPH; e = expr; END; d = defn; { DGraph (e, d) }
	| GRAPH; e = expr; END; { DGraph (e, DEnd) }
	| GRAPH; e = expr; { DGraph (e, DEnd) }
  | s = NAME; COLON; PROMPT; END; d = defn; { DPrompt (s, d) }
	| s = NAME; COLON; PROMPT; END; { DPrompt (s, DEnd) }
	| s = NAME; COLON; PROMPT; { DPrompt (s, DEnd) }
	| s = NAME; COLON; e = expr; END; d = defn; { DAssign (s, e, d) }
	| s = NAME; COLON; e = expr; END; { DAssign (s, e, DEnd) }
	| s = NAME; COLON; e = expr; { DAssign (s, e, DEnd) }
	| m = expr; LBRACKET; e1 = expr; COMMA; e2 = expr; RBRACKET; COLON; 
	  e3 = expr; END; d = defn; { DMatrixSet (m, e1, e2, e3, d) }
	| m = expr; LBRACKET; e1 = expr; COMMA; e2 = expr; RBRACKET; COLON; 
	  e3 = expr; END; { DMatrixSet (m, e1, e2, e3, DEnd) }
	| m = expr; LBRACKET; e1 = expr; COMMA; e2 = expr; RBRACKET; COLON; 
	  e3 = expr; { DMatrixSet (m, e1, e2, e3, DEnd) }
	| DISP; e = expr; END; d = defn; { DDisp (e, d) }
	| DISP; e = expr; END; { DDisp (e, DEnd) }
	| DISP; e = expr; { DDisp (e, DEnd) }

  | IF; e = expr; END; THEN; d1 = short_defn; END; ELSE; d2 = short_defn; END; d3 = defn; { DIf (e, d1, d2, d3) }
	| IF; e = expr; END; THEN; d1 = short_defn; ELSE; d2 = short_defn; END; d3 = defn; { DIf (e, d1, d2, d3) }
	| IF; e = expr; END; THEN; d1 = short_defn; END; ELSE; d2 = short_defn; END; { DIf (e, d1, d2, DEnd) }
	| IF; e = expr; END; THEN; d1 = short_defn; ELSE; d2 = short_defn; END; { DIf (e, d1, d2, DEnd) }
	| IF; e = expr; END; THEN; d1 = short_defn; END; ELSE; d2 = short_defn; { DIf (e, d1, d2, DEnd) }
	| IF; e = expr; END; THEN; d1 = short_defn; ELSE; d2 = short_defn; { DIf (e, d1, d2, DEnd) }
  | IF; e = expr; THEN; d1 = short_defn; END; ELSE; d2 = short_defn; END; d3 = defn; { DIf (e, d1, d2, d3) }
	| IF; e = expr; THEN; d1 = short_defn; ELSE; d2 = short_defn; END; d3 = defn; { DIf (e, d1, d2, d3) }
	| IF; e = expr; THEN; d1 = short_defn; END; ELSE; d2 = short_defn; END; { DIf (e, d1, d2, DEnd) }
	| IF; e = expr; THEN; d1 = short_defn; ELSE; d2 = short_defn; END; { DIf (e, d1, d2, DEnd) }
	| IF; e = expr; THEN; d1 = short_defn; END; ELSE; d2 = short_defn; { DIf (e, d1, d2, DEnd) }
	| IF; e = expr; THEN; d1 = short_defn; ELSE; d2 = short_defn; { DIf (e, d1, d2, DEnd) }

  | IF; e = expr; END; THEN; d1 = short_defn; END; d = defn; { DIf (e, d1, DEnd, d)}
	| IF; e = expr; END; THEN; d1 = short_defn; END; { DIf (e, d1, DEnd, DEnd)}
	| IF; e = expr; END; THEN; d1 = short_defn; { DIf (e, d1, DEnd, DEnd)}
	| IF; e = expr; THEN; d1 = short_defn; END; d = defn; { DIf (e, d1, DEnd, d)}
	| IF; e = expr; THEN; d1 = short_defn; END; { DIf (e, d1, DEnd, DEnd)}
	| IF; e = expr; THEN; d1 = short_defn; { DIf (e, d1, DEnd, DEnd)}

  | CLASS; n = NAME; COLON; xs = nonempty_list(ident); ARROW; LCURLY; ad = defn; RCURLY; END; d = defn; { DDefClass (n, xs, ad, d) }
	| CLASS; n = NAME; COLON; xs = nonempty_list(ident); ARROW; LCURLY; ad = defn; RCURLY; d = defn; { DDefClass (n, xs, ad, d) }
	| CLASS; n = NAME; COLON; xs = nonempty_list(ident); ARROW; LCURLY; ad = defn; RCURLY; END; { DDefClass (n, xs, ad, DEnd) }
	| CLASS; n = NAME; COLON; xs = nonempty_list(ident); ARROW; LCURLY; ad = defn; RCURLY; { DDefClass (n, xs, ad, DEnd) }
	| CLASS; n = NAME; COLON; ARROW; LCURLY; ad = defn; RCURLY; END; d = defn; { DDefClass (n, [], ad, d) }
	| CLASS; n = NAME; COLON; ARROW; LCURLY; ad = defn; RCURLY; d = defn; { DDefClass (n, [], ad, d) }
	| CLASS; n = NAME; COLON; ARROW; LCURLY; ad = defn; RCURLY; END; { DDefClass (n, [], ad, DEnd) }
	| CLASS; n = NAME; COLON; ARROW; LCURLY; ad = defn; RCURLY; { DDefClass (n, [], ad, DEnd) }
	
	| STRUCT; n = NAME; COLON; xs = nonempty_list(ident); ARROW; LCURLY; ad = obj_defn; RCURLY; END; d = defn; { DDefStruct (n, xs, ad, d) }
	| STRUCT; n = NAME; COLON; xs = nonempty_list(ident); ARROW; LCURLY; ad = obj_defn; RCURLY; d = defn; { DDefStruct (n, xs, ad, d) }
	| STRUCT; n = NAME; COLON; xs = nonempty_list(ident); ARROW; LCURLY; ad = obj_defn; RCURLY; END; { DDefStruct (n, xs, ad, DEnd) }
	| STRUCT; n = NAME; COLON; xs = nonempty_list(ident); ARROW; LCURLY; ad = obj_defn; RCURLY; { DDefStruct (n, xs, ad, DEnd) }
	| STRUCT; n = NAME; COLON; ARROW; LCURLY; ad = obj_defn; RCURLY; END; d = defn; { DDefStruct (n, [], ad, d) }
	| STRUCT; n = NAME; COLON; ARROW; LCURLY; ad = obj_defn; RCURLY; d = defn; { DDefStruct (n, [], ad, d) }
	| STRUCT; n = NAME; COLON; ARROW; LCURLY; ad = obj_defn; RCURLY; END; { DDefStruct (n, [], ad, DEnd) }
	| STRUCT; n = NAME; COLON; ARROW; LCURLY; ad = obj_defn; RCURLY; { DDefStruct (n, [], ad, DEnd) }
	| n = NAME; DOLLAR; s = NAME; COLON; e = expr; END; d = defn; { DStructSet (n, s, e, d) }
	| n = NAME; DOLLAR; s = NAME; COLON; e = expr; END; { DStructSet (n, s, e, DEnd) }
	| n = NAME; DOLLAR; s = NAME; COLON; e = expr; { DStructSet (n, s, e, DEnd) }
	| n = NAME; DOT; s = NAME; COLON; e = expr; END; d = defn; { DObjSet (n, s, e, d) }
	| n = NAME; DOT; s = NAME; COLON; e = expr; END; { DObjSet (n, s, e, DEnd) }
	| n = NAME; DOT; s = NAME; COLON; e = expr; { DObjSet (n, s, e, DEnd) }
	| GOTO; s = NAME; END; d = defn; { DGoto (s, d) }
	| GOTOSUB; s = NAME; END; d = defn; { DGotoSub (s, d) }
	| LBL; s = NAME; END; d = defn; { DLabel (s, d) }
	| GOTO; s = NAME; END; { DGoto (s, DEnd) }
	| GOTO; s = NAME; { DGoto (s, DEnd) }
	| GOTOSUB; s = NAME; END; { DGotoSub (s, DEnd) }
	| GOTOSUB; s = NAME; { DGotoSub (s, DEnd) }
	| LBL; s = NAME; END; { DLabel (s, DEnd) }
	| LBL; s = NAME; { DLabel (s, DEnd) }
	| OUTPUT; LPAREN; e1 = expr; COMMA; e2 = expr; COMMA; 
	  e3 = expr; RPAREN; END; d = defn; { DOutput (e1, e2, e3, Val (Color Black), d) }
	| OUTPUT; LPAREN; e1 = expr; COMMA; e2 = expr; COMMA; 
	  e3 = expr; RPAREN; END; { DOutput (e1, e2, e3, Val (Color Black), DEnd) }
	| OUTPUT; LPAREN; e1 = expr; COMMA; e2 = expr; COMMA; 
	  e3 = expr; RPAREN; { DOutput (e1, e2, e3, Val (Color Black), DEnd) }
	| OUTPUT; LPAREN; e1 = expr; COMMA; e2 = expr; COMMA; 
	  e3 = expr; COMMA; e4 = expr; RPAREN; END; d = defn; { DOutput (e1, e2, e3, e4, d) }
	| OUTPUT; LPAREN; e1 = expr; COMMA; e2 = expr; COMMA; 
	  e3 = expr; COMMA; e4 = expr; RPAREN; END; { DOutput (e1, e2, e3, e4, DEnd) }
	| OUTPUT; LPAREN; e1 = expr; COMMA; e2 = expr; COMMA; 
	  e3 = expr; COMMA; e4 = expr; RPAREN; { DOutput (e1, e2, e3, e4, DEnd) }
	| LINE LPAREN e1 = expr; COMMA; e2 = expr; COMMA; e3 = expr; COMMA; e4 = expr;
	  RPAREN; END; d = defn; { DLine (e1, e2, e3, e4, d) }
	| LINE LPAREN e1 = expr; COMMA; e2 = expr; COMMA; e3 = expr; COMMA; e4 = expr;
	  RPAREN; END; { DLine (e1, e2, e3, e4, DEnd) }
	| LINE LPAREN e1 = expr; COMMA; e2 = expr; COMMA; e3 = expr; COMMA; e4 = expr;
	  RPAREN; { DLine (e1, e2, e3, e4, DEnd) }
	| RETURN; e = expr; END; d = defn; { DReturn (e, d) }
	| RETURN; e = expr; END; { DReturn (e, DEnd) }
	| RETURN; e = expr; { DReturn (e, DEnd) }
	| RETURN; END; d = defn; { DReturn (Val Null, d) }
	| RETURN; d = defn; { DReturn (Val Null, d) }
	| RETURN; END; { DReturn (Val Null, DEnd) }
	| RETURN; { DReturn (Val Null, DEnd) }
	| WHILE; e = expr; LCURLY; d1 = defn; RCURLY; END; d2 = defn; { DWhile (e, d1, d2) }
	| WHILE; e = expr; LCURLY; d1 = defn; RCURLY; END; { DWhile (e, d1, DEnd) }
	| WHILE; e = expr; LCURLY; d1 = defn; RCURLY; { DWhile (e, d1, DEnd) }
	| WHILE; e = expr; LCURLY; END; d1 = defn; END; RCURLY; END; d2 = defn; { DWhile (e, d1, d2) }
	| WHILE; e = expr; LCURLY; END; d1 = defn; END; RCURLY; END; { DWhile (e, d1, DEnd) }
	| WHILE; e = expr; LCURLY; END; d1 = defn; END; RCURLY; { DWhile (e, d1, DEnd) }
	| WHILE; e = expr; LCURLY; d1 = defn; END; RCURLY; END; d2 = defn; { DWhile (e, d1, d2) }
	| WHILE; e = expr; LCURLY; d1 = defn; END; RCURLY; END; { DWhile (e, d1, DEnd) }
	| WHILE; e = expr; LCURLY; d1 = defn; END; RCURLY; { DWhile (e, d1, DEnd) }
	| WHILE; e = expr; LCURLY; END; d1 = defn; RCURLY; END; d2 = defn; { DWhile (e, d1, d2) }
	| WHILE; e = expr; LCURLY; END; d1 = defn; RCURLY; END; { DWhile (e, d1, DEnd) }
	| WHILE; e = expr; LCURLY; END; d1 = defn; RCURLY; { DWhile (e, d1, DEnd) }
	| END; d = defn; { d } 
	| d = defn; END; { d }
  | END; { DEnd }
	| FUN; n = ident; COLON; ARROW; LCURLY; d = defn; RCURLY; END; d2 = defn;
		{ DFunction (n, [], d, d2) }
	| FUN; n = ident; COLON; ARROW; LCURLY; END; d = defn; END; RCURLY; END; d2 = defn;
		{ DFunction (n, [], d, d2) }
	| FUN; n = ident; COLON; ARROW; LCURLY; d = defn; RCURLY;
		{ DFunction (n, [], d, DEnd) }
	| FUN; n = ident; COLON; ARROW; LCURLY; END; d = defn; END; RCURLY;
		{ DFunction (n, [], d, DEnd) }
	| FUN; n = ident; COLON; ARROW; LCURLY; d = defn; RCURLY; END;
		{ DFunction (n, [], d, DEnd) }
	| FUN; n = ident; COLON; ARROW; LCURLY; END; d = defn; END; RCURLY; END;
		{ DFunction (n, [], d, DEnd) }

	| FUN; n = ident; COLON; xs = nonempty_list(ident); ARROW; LCURLY; d = defn; RCURLY; END; d2 = defn;
		{ if has_dups xs
			then $syntaxerror (* duplicate argument names *)
			else DFunction (n, xs, d, d2) }
	| FUN; n = ident; COLON; xs = nonempty_list(ident); ARROW; LCURLY; END; d = defn; END; RCURLY; END; d2 = defn;
		{ if has_dups xs
			then $syntaxerror (* duplicate argument names *)
			else DFunction (n, xs, d, d2) }
	| FUN; n = ident; COLON; xs = nonempty_list(ident); ARROW; LCURLY; d = defn; RCURLY;
		{ if has_dups xs
			then $syntaxerror (* duplicate argument names *)
			else DFunction (n, xs, d, DEnd) }
	| FUN; n = ident; COLON; xs = nonempty_list(ident); ARROW; LCURLY; END; d = defn; END; RCURLY;
		{ if has_dups xs
			then $syntaxerror (* duplicate argument names *)
			else DFunction (n, xs, d, DEnd) }
	| FUN; n = ident; COLON; xs = nonempty_list(ident); ARROW; LCURLY; d = defn; RCURLY; END;
		{ if has_dups xs
			then $syntaxerror (* duplicate argument names *)
			else DFunction (n, xs, d, DEnd) }
	| FUN; n = ident; COLON; xs = nonempty_list(ident); ARROW; LCURLY; END; d = defn; END; RCURLY; END;
		{ if has_dups xs
			then $syntaxerror (* duplicate argument names *)
			else DFunction (n, xs, d, DEnd) }
	;

short_defn:
  | GRAPH; e = expr; { DGraph (e, DEnd) }
	| DISP; e = expr; { DDisp (e, DEnd) }
	| s = NAME; COLON; PROMPT; { DPrompt (s, DEnd) }
	| s = NAME; COLON; e = expr; { DAssign (s, e, DEnd) }
	| n = NAME; DOLLAR; s = NAME; COLON; e = expr; { DStructSet (n, s, e, DEnd) }
	| n = NAME; DOT; s = NAME; COLON; e = expr; { DObjSet (n, s, e, DEnd) }
	| GOTOSUB; s = NAME; { DGotoSub (s, DEnd) }
	| GOTO; s = NAME; { DGoto (s, DEnd) }
	| m = expr; LBRACKET; e1 = expr; COMMA; e2 = expr; RBRACKET; COLON; 
	  e3 = expr; { DMatrixSet (m, e1, e2, e3, DEnd) }
	| OUTPUT; LPAREN; e1 = expr; COMMA; e2 = expr; COMMA; e3 = expr; RPAREN; 
	  { DOutput (e1, e2, e3, Val (Color Black), DEnd) }
	| OUTPUT; LPAREN; e1 = expr; COMMA; e2 = expr; COMMA; 
	  e3 = expr; COMMA; e4 = expr; RPAREN; { DOutput (e1, e2, e3, e4, DEnd) }
	| RETURN; e = expr; { DReturn (e, DEnd) }
	| RETURN; END; { DReturn (Val Null, DEnd) }
	| LPAREN; d = defn; RPAREN; { d }
	| END; { DEnd }
	;

obj_defn:
	| s = NAME; COLON; e = expr; END; d = obj_defn; { DAssign (s, e, d) }
	| s = NAME; COLON; e = expr; END; { DAssign (s, e, DEnd) }
	| s = NAME; COLON; e = expr; { DAssign (s, e, DEnd) }
	| END; d = obj_defn; { d } 
	| d = obj_defn; END; { d }
  | END; { DEnd }
	;