e = expression, d = definition, var = variable name, m = matrix

xs = expression list (can be empty)

od = object definition, sd = short definition


##Definitions: 
GRAPH e **(** END **(** d **))**

var : PROMPT **(** END **(** d **))**

var : e **(** END **(** d **))**

m[e1,e2] : e3 **(** END **(** d **))**

DISP e **(** END **(** d **))**

IF e **(** END **)** THEN sd1 **(** END **)** **(** ELSE sd2 **)** **(** 

END **(** d **))**

STRUCT struct : xs -> { od } **(** END **(** d **))**

built$var : e **(** END **(** d **))**

GOTO lbl **(** END **(** d **))**

LBL lbl **(** END **(** d **))**

GOSUB lbl **(** END **(** d **))**

OUTPUT(e_x, e_y, e_out **(**, e_c **)**) **(** END **(** d **))**

LINE(e_x1, e_y1, e_x2, e_y2) **(** END **(** d **))**

RETURN **(** e **)** **(** END **(** d **))**

WHILE e {**(** END **)** d1 **(** END **)**} **(** END **(** d **))**

FUN name : xs -> {**(** END **)** d1 **(** END **)**} **(** END **(** d **))**

- Note: either { END d1 END } or { d1 } is acceptable, but no other combination of END is possible

**(** END **)** d **(** END **)**

END

##Object definitions:

var : e **(** END **(** od **))**

**(** END **)** od **(** END **)**

END

##Short definitions:

GRAPH e

DISP e

var : PROMPT

var : e

built$var : e

GOSUB lbl

GOTO lbl

m[e1,e2] : e3

OUTPUT(e_x, e_y, e_out **(**, e_c **)**)

RETURN **(** e **)**

( d )

END