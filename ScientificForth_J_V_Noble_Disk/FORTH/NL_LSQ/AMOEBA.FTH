\ FUNCTION MINIMIZATION BY THE SIMPLEX METHOD
\ VERSION OF 18:57:29  5/31/1990

TASK AMOEBA

\ ------------------------------------------------ FINITE STATE MACHINE
: WIDE   0 ;
: STATE.TAB:   ( width 0 -- )   CREATE   ,  ,   ]   DOES>   ( col# -- )
               UNDER  D@        ( -- adr col# width state )
               *  +   1+  4*    ( -- adr offset )
               OVER +           ( -- adr adr' )
               DUP@  SWAP 2+    ( -- adr [adr']  adr'+2 )
               EXECUTE@         ( -- adr [adr']  state')
               ROT !   EXECUTE   ;

0 CONSTANT >0
1 CONSTANT >1
2 CONSTANT >2
3 CONSTANT >3
4 CONSTANT >4
\ -------------------------------------------- END FINITE STATE MACHINE

\ --------------------------------------------------- FUNCTION NOTATION
    VARIABLE <F>
    : USE(   [COMPILE]  '   CFA  <F> ! ;
    : F(X)   <F>  EXECUTE@  ;
    BEHEAD'  <F>
\ ----------------------------------------------- END FUNCTION NOTATION

\ ----------------------------------------------------- DATA STRUCTURES
2 VAR Ndim
0 VAR N
0 VAR N.max
0 VAR I.worst
0 VAR I.best

CREATE SIMPLEX{{  Ndim   4  ( bytes )  *  Ndim 1+  ( # points )  *  ALLOT
CREATE F{  Ndim 1+  4  ( bytes )  *   ALLOT    \ residuals

DVARIABLE  Residual
DVARIABLE  Residual'
DVARIABLE  Epsilon

CREATE X{  Ndim     4  ( bytes )  *   ALLOT    \ trial point
CREATE XP{ Ndim     4  ( bytes )  *   ALLOT    \ 2nd trial point
CREATE Y{  Ndim     4  ( bytes )  *   ALLOT    \ geocenter

: }    ( adr n -- adr + 4n)   4*  +  ;
: }}   ( adr m n -- adr + [m*Ndim + n]*4 )   SWAP  Ndim *  +   }  ;
\ ------------------------------------------------- END DATA STRUCTURES

\ -------------------------------------------------------- ACTION WORDS
: RESIDUALS   Ndim 1+  0  DO  SIMPLEX{{ I 0 }}  F(X)  F{ I }  R32!
              LOOP  ;

: CENTER  ( -- )   FINIT  Ndim S->F
        Ndim 0  DO                            \ loop over components
                                              ( :: -- N )
             F=0                              \ loop over vectors
             Ndim 1+ 0  DO   I I.worst  <>  IF
                  SIMPLEX{{ I J }}  R32@  F+   THEN
             LOOP   FOVER  F/   Y{ I }  R32!  \ put away
        LOOP    FDROP  ;

: DONE!    CR  ." We're finished."  ;
: TOO.MANY   CR  ." Too many iterations."  ;

: V.MOVE   ( src.adr  dest.adr -- )
        Ndim 0  DO   OVER  I }   OVER  I }  2 MOVE   LOOP   DDROP  ;

: STORE   ( adr1 adr2 -- )   0 }  SIMPLEX{{ I.worst 0 }} V.MOVE
                             F{ I.worst }  2  MOVE  ;

: ST0  Residual  X{  STORE ;
: ST1  Residual'  XP{  STORE ;

: New.F     X{   F(X)   Residual   R32!  ;

FIND CPDUP  0= ?( : CPDUP  FOVER FOVER ;  )

: Worst.Point   FINIT    F{ 0 } R32@   0
                Ndim 1+  1  DO   F{ I }  R32@  CPDUP   F<
                     IF   FSWAP  DROP  I   THEN   FDROP
                LOOP   FDROP  ;

: Best.Point    FINIT    F{ 0 } R32@   0
                Ndim 1+  1  DO   F{ I }  R32@  CPDUP   F>
                     IF   FSWAP  DROP  I   THEN   FDROP
                LOOP   FDROP  ;

: 2worst        FINIT  F{ I.best }  R32@  0
                Ndim 1+  0  DO
                     I I.worst <>
                     IF   F{ I }  R32@   CPDUP   F<=
                          IF   FSWAP  DROP  I   THEN  FDROP
                     THEN
                LOOP   FDROP  ;

: SCALE     ( :: scale -- )
            Ndim 0  DO  DUP  I }  R32@       ( :: -- sc x )
                        Y{ I }  R32@  FUNDER  F-   ( :: -- sc y x-y)
                        FROT  FUNDER  F*           ( :: -- y sc [x-y]*sc)
                        FROT  F+                   ( :: -- sc y+[x-y]*sc)
                        X{ I }  R32!
            LOOP  DROP  FDROP  New.F  ;

FIND F=1/2  0= ?( : F=1/2  F=1 FNEGATE F=1 FSCALE FPLUCK ; )
FIND F=2    0= ?( : F=2    F=1 FDUP FSCALE FPLUCK ;  )
\ -------------------------------------------------- DEBUGGING CODE
0 VAR DBG
: DEBUG-ON  -1 IS DBG ;
: DEBUG-OFF 0 IS DBG ;

: .V   3 SPACES  Ndim 0  DO   DUP I }  R32@  F.   LOOP  DROP  ;
: .M   ( -- )   Ndim 1+ 0  DO  DBG  CR  IF  I .  2 SPACES  THEN
                     SIMPLEX{{ I 0 }}  .V
                     DBG  IF  F{ I }  R32@ F.  THEN
                LOOP  CR
                DBG  IF CR  X{ .V   Residual R32@ F. THEN ;

: .F   Ndim 1+  0 DO  CR   F{ I }   R32@  F.   LOOP ;
\ ---------------------------------------------- END DEBUGGING CODE
: REFL      CENTER   F=1  FNEGATE  SIMPLEX{{ I.worst 0 }}
            SCALE    DBG IF  CR ." REFLECTING"  THEN  .M    ;


: DBL     Residual Residual'  2 MOVE       \ swap points, resid.
          X{  XP{  V.MOVE
          FINIT  F=2   FNEGATE  X{  SCALE
          DBG IF  CR ." DOUBLING"  THEN  .M  ;

: HALVE   FINIT  F=1/2   SIMPLEX{{ I.worst 0 }}  SCALE
          DBG IF  CR ." HALVING"  THEN  .M  ;

: SHRINK   SIMPLEX{{ I.best 0 }}  Y{  V.MOVE
           Ndim 1+  0  DO
                   Ndim 0  DO  SIMPLEX{{ J I }}  DUP
                            R32@   Y{ I }  R32@    FUNDER  F-
                            F=1/2  F*   F+     R32!
                   LOOP
           LOOP   RESIDUALS
           DBG  IF  CR ." SHRINKING"   THEN  .M  ;
\ ---------------------------------------------------- END ACTION WORDS

SYNONYM >N NEXT
\ ---------------------------------------------------------- TEST WORDS
: (test)   FINIT  Residual  R32@  F{ SWAP }  R32@   F<  NOT ;
: best>=    I.best   (test)  ;
: worst>=   I.worst  (test)  ;
: 2worst>=  2worst   (test)  ;

: done?   F{ I.worst } R32@  F{ I.best }  R32@  CPDUP  F-  F2*
          F-ROT  F+  F/   FABS   Epsilon  R32@   F<  ;
\ ------------------------------------------------------ END TEST WORDS

4 WIDE STATE.TAB:  New.Point
\ Input:   |  best<   |  2worst< |  worst<  |  worst>= |
\ State:   ---------------------------------------------
  ( 0 )       DBL >1     ST0  >4    ST0  >2     >N  >2
  ( 1 )       ST0 >4     ST1  >4    ST1  >4    ST1  >4
  ( 2 )       >N  >4     >N   >4    HALVE >3   HALVE >3
  ( 3 )       ST0 >4     ST0  >4    ST0  >4    SHRINK >4    ;

: cat->col#  ( -- col#)  1  2worst>=   -
                         worst>=  -     best>=  AND  ;

: )MINIMIZE   ( n.iter --  :: error -- )
       IS N.max   Epsilon R32!   0 IS N
       RESIDUALS  Worst.Point IS I.worst
                  Best.Point IS I.best
       BEGIN    done?  NOT    N N.max  <  AND   WHILE
            Worst.Point IS I.worst
            Best.Point  IS I.best
            REFL   ' New.Point  0!
            BEGIN   cat->col#    New.Point
                    ' New.Point @   4 =    UNTIL
            N 1+  IS N
       REPEAT   DONE!  ;


\ EXAMPLE FUNCTIONS
: F1  ( adr -- :: F )  DUP  0 }   R32@  F**2   1 }  R32@  F**2   F2*  F+  ;
\ f1 = x^2 + 2*y^2

: F2        ( adr -- :: F )  DUP  0 }   R32@  FDUP F**2  1 }  R32@  F**2
            F2* F+  36 S->F  F-  F**2  F2/  FSWAP  6 S->F  F*  F-  ;
\  f2 = .5 * (x * x + 2 * y * y - 36) ^ 2 - 6 * x

: F3        ( adr -- :: F )  DUP DUP  0 }   R32@  FDUP F**2  1 }  R32@  F**2
          F2* F+  36 S->F  F-  F**2  F2/  FSWAP  6 S->F  F*  F-
          DUP 1 } R32@  0 } R32@  F/  FATAN  F2*  FCOS  F**2  F*  ;
\ f3 = [(x^2 2*y^2 - 36)^2 - 6*x] * cos(2*atan(y/x)) ^ 2

\ Usage:    USE( MYFUNC  10  1.E-3 )MINIMIZE

FLOATS

5. SIMPLEX{{ 0 0 }} R32!     -3.  SIMPLEX{{ 0 1 }} R32!
5. SIMPLEX{{ 1 0 }} R32!     3.  SIMPLEX{{ 1 1 }} R32!
-10. SIMPLEX{{ 2 0 }} R32!    1.  SIMPLEX{{ 2 1 }} R32!

DEBUG-ON
