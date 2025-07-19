CR .( COPYRIGHT 1987  JULIAN V. NOBLE )
CR .(                 105 POWHATAN CIRCLE )
CR .(                 CHARLOTTESVILLE, VA 22901 ) CR
CR .(                 FOR NON-COMMERCIAL USE ONLY ) CR

TASK INTEGRAL
FIND  C.  0= ?( FLOAD COMPLEX )
FIND REAL*4 0= ?(((  0 CONSTANT REAL*4   1 CONSTANT REAL*8
                   2 CONSTANT COMPLEX  3 CONSTANT DCOMPLEX

CODE #BYTES  BX 3 IW AND.  CX BX MOV.  BX 1 IW AND.   BX INC.
             BX 1 SHL.  BX 1 SHL.   CX 1 SAR.    BX 2 SHL.  END-CODE
CODE STEP/2   AX BX MOV.  AX INC. AX INC. AX PUSH. BX [BX] MOV.  END-CODE
: SCALAR  ( type -- )   CREATE  DUP ,   #BYTES  ALLOT
    DOES>  LISTS @  SWAP  STEP/2 ;  ( -- seg off t)
: SCALARS   ( n t -- )  SWAP  0 DO   DUP   SCALAR   LOOP  DROP ;
CASE: G!   R32!L  R64!L   CP!L   DCP!L  ;CASE
CASE: G@   R32@L  R64@L   CP@L   DCP@L  ;CASE  )))

FIND 1ARRAY  0= ?( FLOAD MATRIX.HSF )

VARIABLE <F>
: USE(   [COMPILE]  '   CFA  <F> ! ;
: F(X)   <F>  EXECUTE@  ;
BEHEAD'  <F>

: from ;   : to  ;   : error ;

4 S->F  3 S->F  F/  FCONSTANT F=4/3

0 VAR N   0 VAR type               \ BEHEADING starts here

20 LONG REAL*8 1ARRAY X{
20 LONG REAL*4  1ARRAY E{
20 LONG DCOMPLEX 1ARRAY F{
20 LONG DCOMPLEX 1ARRAY I{

2 DCOMPLEX SCALARS old.I  final.I

: inc.N   N 1+  IS N  ;
: dec.N   N 2-  IS N  ;

: )integral  ( n --)               \ trapezoidal rule
    X{ OVER } G@   X{ OVER 1- }  G@
    F-  F2/
    F{ OVER } G@   F{ OVER 1- }  G@
    type 2 AND  IF C+  FROT  C*F  ELSE  F+  F*  THEN
    I{ SWAP 1- } G!  ;

: INITIALIZE   1 IS N   IS  type
     type F{ !  type I{ !  type 1 AND  X{ !
     type ' old.I !   type  ' final.I !
     E{ 0 } G!   X{ 1 } G!   X{ 0 } G!
     X{ 0 } G@   F(X)   F{ 0 } G!
     X{ 1 } G@   F(X)   F{ 1 } G!
     N )integral
     type 2 AND  IF F=0 THEN  F=0  final.I G!  FINIT  ;

: check.N       N 19 >  ABORT" Too many subdivisions!"  ;
: E/2   E{ N 1- }  G@  F2/  E{ N 1- }  G! ;
: }move.down    ( adr n --)
        }   #BYTES   >R   DDUP  R@ +   R>  CMOVEL ;
: MOVE.DOWN    E{ N 1- }move.down
               X{ N }move.down
               F{ N }move.down  ;

: new.X    X{ N } G@   X{ N 1- } G@  F+  F2/
           FDUP  X{ N } G!  ;

  : GF.  1 > IF FSWAP  E. THEN  E. ;
  : F@.  DUP>R  G@  R> GF. ;
  : .STACKS    CR  ." N"  8 CTAB  ." X"
               19 CTAB  ." Re[F(X)]"   31 CTAB ." Im[F(X)]"
               45 CTAB  ." Re[I]"      57 CTAB ." Im[I]"
               71 CTAB ." E"
              N 2+ 0 DO
                 CR I .  3 CTAB  X{ I } F@.  16 CTAB  F{ I } F@.
                 42 CTAB  I{ I } F@.   65 CTAB  E{ I }  F@.
              LOOP  CR
                 5 SPACES ." old.I ="  old.I F@.
                 5 SPACES  ." final.I =" final.I  F@.  CR ;

CASE: <DEBUG>   NEXT .STACKS  ;CASE
0 VAR (DEBUG)
: DEBUG-ON   1 IS (DEBUG)  5 #PLACES ! ;
: DEBUG-OFF  0 IS (DEBUG)  7 #PLACES ! ;
: DEBUG  (DEBUG) <DEBUG> ;

: SUBDIVIDE    check.N     E/2   MOVE.DOWN
        I{ N 1- }  DROP  old.I  #BYTES  CMOVEL
        new.X  F(X)  F{ N } G!
        N )integral   N 1+ )integral  ;


: CONVERGED?   ( 87: -- I[N]+I'[N-1]-I[N-1]   -- f)
               I{ N } G@  I{ N 1- } G@   old.I G@
               type 2 AND IF C- C+  CPDUP  CABS  ELSE F- F+ FDUP  FABS  THEN
               E{ N 1- } G@   F2*    F<  ;

: INTERPOLATE  ( 87: I[N]+I'[N-1]-I[N-1] -- )
      F=4/3  type  2 AND  IF  C*F  ELSE  F*  THEN
      old.I G@   final.I G@   type 2 AND  IF C+ C+  ELSE F+ F+  THEN
      final.I G!  ;
                                       \ BEHEADing ends here

: )INTEGRAL    ( :: I{ F{ A B ERR -- I[A,B])
     INITIALIZE
     BEGIN   N 0>   WHILE
        SUBDIVIDE   DEBUG
        CONVERGED?   inc.N
        IF  INTERPOLATE  dec.N
        ELSE  type 2 AND IF FDROP THEN   FDROP    THEN
     REPEAT   final.I  G@  ;
\ BEHEAD'' N  INTERPOLATE
CR   ." Usage:  USE( FN.NAME from A to B error E  TYPE )INTEGRAL "
\ ;

