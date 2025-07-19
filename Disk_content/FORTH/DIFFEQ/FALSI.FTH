\ Equation solving by regula falsi ("false approach")
\ cum binary search
\ -- version of 2/25/1993

\ The equation to be solved is f(x) = 0.
\ Suppose a and b bracket the true root, i.e. f(a) * f(b) < 0:

\ Then let A = f(a), B = f(b) and pass a straight line through the
\ points (x,y) = (a,A) and (b,B).

\ This line has the form

\      y = A * (b - x)/(b-a)  +  B * (a - x)/(a - b)

\ We now find the x-axis intercept, c = (A*b - B*a)/(A - B)
\    and evaluate the function at that point: C = f(c).

\    If A * C > 0 then the true root lies between c and b, whereas
\    if A * C < 0 it lies between a and c. We continue by iteration
\    until the root is bracketed with sufficient accuracy.

\ Say  " USE( fn.name  % a  % b  % precision )FALSI

FIND F"  0= ?( FLOAD FORTRAN.FTH )

0 VAR <F>
: USE(    [COMPILE]   CFA'   IS <F>  ;
: f(x)    <F>  EXECUTE  ;
\ BEHEAD' <F>

7 REAL*8 SCALARS  a A  b B  c C   precision

: SAME.SIGN?   ( seg off type seg' off' type' -- f)  G@L  G@L  F*  F0>  ;

: INITIALIZE  precision G!L  b G!L  a G!L   \ store args
              FS.INIT                       \ init IFSTACK
              a >FS  f(x)  A FS>            \ calculate residual
              b >FS  f(x)  B FS>            \ calculate residual
              A  B  SAME.SIGN?   ABORT" Even number of roots."   ;

: x'    F" c = (A*b - B*a) / (A - B) "  ;  \ new guess for root

: APART    ( -- f)
           F" ABS( b - a ) "   FDUP  precision  G@L  F>   FS.DROP  ;

: NEW.C    c >FS   f(x)   C FS>  ;   \ calculate new residual

: C->A     F" A = C"  F" a = c "  ;
: C->B     F" B = C"  F" b = c "  ;

: REVISE   NEW.C                     \ calculate new residual
           A C  SAME.SIGN?
           IF      C->A     F" c = (c + b)/2 "  NEW.C
                   C B SAME.SIGN?   IF   C->B   THEN
           ELSE    C->B     F" c = (c + a)/2 "  NEW.C
                   C A SAME.SIGN?   IF   C->A   THEN
           THEN   ;

: )FALSI  ( 87: a b precision -- :: root)   INITIALIZE
      BEGIN   APART   WHILE    x'  REVISE    REPEAT   ;

