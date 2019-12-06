- e = expression, d = definition, var = variable name, m = matrix
- xs = expression list (can be empty)
- od = object definition, sd = short definition

Choice expressions:
- INTEGRAL((e_left, e_right), e_func)
- DERIVATIVE(e_val, e_func)
- RANDINT(e_low, e_high)
  - doesn't include the high bound
- e_guard ? e1 : e2
  - ternary operator, returns the result of evaluating e1 if e_guard is true, returns the result of evaluating e2 if e_guard is false
- MATRIX(e_x, e_y)
- VARMAT(e_x, e_y)
  - similar to a matrix, but can hold any value type in every cell (not just a number) and can't perform matrix operations (addition, multiplication, etc)
- built$var
  - returns the member variable var of a built built
- func <- ( xs )
  - function application
- struct <- ( xs )
  - built initialization/struct constructor application
- m[e1,e2]
  - calls on a cell of a varmat or a matrix


Definitions: 
- GRAPH e **(** END **(** d **))**
  - graphs function e on a -10 10 -10 10 grid, but with no visible axes
- var : PROMPT **(** END **(** d **))**
- var : e **(** END **(** d **))**
- m[e1,e2] : e3 **(** END **(** d **))**
- DISP e **(** END **(** d **))**
- IF e **(** END **)** THEN sd1 **(** END **)** **(** ELSE sd2 **)** **(** - END **(** d **))**
- STRUCT struct : xs -> { od } **(** END **(** d **))**
- built$var : e **(** END **(** d **))**
- GOTO lbl **(** END **(** d **))**
- LBL lbl **(** END **(** d **))**
- GOSUB lbl **(** END **(** d **))**
- OUTPUT(e_x, e_y, e_out **(**, e_c **)**) **(** END **(** d **))**
  - output window is broken into a 0-29 inclusive grid along x and y; default color is black
- LINE(e_x1, e_y1, e_x2, e_y2) **(** END **(** d **))**
  - coordinates are given by the grid; maximum visible values 0-30 for x and y
- RETURN **(** e **)** **(** END **(** d **))**
- WHILE e {**(** END **)** d1 **(** END **)**} **(** END **(** d **))**
- FUN name : xs -> {**(** END **)** d1 **(** END **)**} **(** END **(** d **))**
  - Note: either { END d1 END } or { d1 } is acceptable, but no other combination of END is possible
- **(** END **)** d **(** END **)**
- END

Object definitions:
- var : e **(** END **(** od **))**
- **(** END **)** od **(** END **)**
- END

Short definitions:
- GRAPH e
- DISP e
- var : PROMPT
- var : e
- built$var : e
- GOSUB lbl
- GOTO lbl
- m[e1,e2] : e3
- OUTPUT(e_x, e_y, e_out **(**, e_c **)**)
- RETURN **(** e **)**
- ( d )
- END