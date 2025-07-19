4 CONSTANT REAL
: VARIABLES   SWAP  0 DO  DUP  CREATE  ALLOT  LOOP DROP ;
5 REAL VARIABLES T T' H Y TMAX 
0 VAR <F(T)>
SYNONYM  F@ R32@   SYNONYM  F! R32!  SYNONYM  FR/ F\

: F(T)  <F(T)>  EXECUTE ;  ( :: x -- f[x,t] )

: T+H  T F@  H F@  F+ T' F! ;
: INC.T  T' T 4 CMOVE ;
: INC.X   Y F! ;

: FUNDER  FSWAP FOVER ;
: F2/  2 S->F  F/ ;
: XP   ( :: -- x' = { f(x",t'] + f[y,t] }*h/2 + y )
    H F@  Y F@  FUNDER FUNDER  ( :: -- y y h y)
    T F(T)  FOVER F*           ( :: -- y y h f[y,t]*h )
    FROT FOVER F+              ( :: -- y h f[y,t]*h x" )
    T' F(T)  FROT  F*          ( :: -- y f[y,t]*h  f[x",t']*h )
    F+  F2/  F+                ( :: -- x')  ;

: DONE.YET?  T F@  TMAX F@  F>  ;

: ADVANCE  ( :: -- )
    T+H  XP   INC.T  INC.X  ;

: Y(T)  FDUP FDUP F* F*  3 S->F  F/  F=1 F+   FLN ;
: DISPLAY  >FILE  CR  T F@  FDUP  F.  Y F@ F.  Y(T)  F. ;

: RUNGE   CFA IS <F(T)>   BEGIN   DISPLAY  DONE.YET?  NOT
                          WHILE   ADVANCE 
                          REPEAT  ;

\ Usage: ' FNB  RUNGE

\ Example f(x,t)
: FNB  FNEGATE FEXP   F@  F**2   F* ;  ( a[t] -- :: x -- t^2*exp[-x])

\ set up initial cond'x
F=0 T F!   % 0.1 H F!    T Y 4 CMOVE     % 5 TMAX F!
FINIT

5 #PLACES !

$" RUNGE.OUT" MAKE-OUTPUT
' FNB  RUNGE
CLOSE-OUTPUT
DOS" TYPE RUNGE.OUT"


