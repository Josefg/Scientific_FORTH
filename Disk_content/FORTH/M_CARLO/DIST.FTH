TASK DIST

\ defining word for tables of 2**n prn's distributed according to
\ a given cumulative distribution P(x < X(xi)) = xi

FIND FSTART  0= ?( : FSTART  8 0 DO F=0  LOOP  FINIT  ; )

FIND FR-  0=  ?( SYNONYM  FR-  -F- )

FIND F32, 0=  ?( : F32,        HERE-L 4 ALLOT R32! ; )

HEX
1 VAR A   2 VAR B     03FF VAR MX
: RANDOM   A B +  MX OVER U<   IF MX 1+ - THEN
               2* MX OVER U<   IF MX    - THEN  B IS A   DUP IS B ;
: RANDOMIZE  ( seed1 seed2 #bits -- )
           2 MAX 0F MIN 1 SWAP SAL 1- IS MX     MX MOD IS A  MX MOD IS B
             5 0 DO RANDOM DROP LOOP ;
BEHEAD'' A MX
DECIMAL
\ ------------------------------------------------------- auxiliary words
: 2**  0 MAX 15 MIN  1 SWAP ?DUP 0> IF 0 DO  2*  LOOP  THEN  ;

: LOG2   ( 2**N -- N)   0 SWAP
         BEGIN  2/  ?DUP  0>   WHILE  SWAP  1+  SWAP  REPEAT ;

: START.RANDOM  ( adr --)
        @  LOG2  ( 2**N -- N)  598 12987  ROT  RANDOMIZE ;
\ --------------------------------------------------- end auxiliary words

 0 VAR inv.p                     \ for forward reference
: DISTRIBUTION:                  ( N --)
      CREATE                     \ make entry under dist.name
      2**                        ( -- 2**N)
      DUP  ,   0.0 D,            \ save dimensionality
      DUP  1 DO                  \ table will have 2**N entries
           I S->F  DUP  S->F  F/ ( -- xi)
           inv.p  EXECUTE        ( -- X[xi])   F32,
      LOOP   DROP  DOES>  RANDOM  4*  +   2+  R32@ ;
\ usage:   CFA' fn.name IS inv.p        ( fulfill forward ref.)
\          N  DISTRIBUTION  dist.name   ( make dist.)
\ fn.name converts xi to X(xi)

\ ---------------------------------------------------- regula falsi
VARIABLE <F>
: USE(  [COMPILE]  '  CFA  <F>  !  ;  IMMEDIATE
: F(X)  <F>  EXECUTE@  ;
\ BEHEAD'  <F>

6 REAL*8 SCALARS PRECISION XL XR F(L) F(R) OLDX

: SGN->S   F0<  2* 1+  ( 87: x --  -- sgn[x]) ;

: SAME.SIGN?  ( 87: x -- x  s -- f | true if s=sgn[x])
     FDUP SGN->S  = ;

: INITIALIZE  XR G!  XL FDUP G!  OLDX G!
              PRECISION G!
              XL G@  F(X)  FDUP SGN->S  F(L) G!
              XR G@  F(X)  DUP  SAME.SIGN?
              ABORT" Even number of roots."  F(R) G!  ;

: X'    XL G@  XR G@  F(L) G@  F(R) G@
        CPDUP  F-  F4X
        F*  F-ROT  F*  FR-  F\  ;  ( 87: -- x')

: APART   OLDX G@  FOVER  F- FABS   PRECISION  G@ F> ;
( 87: x' -- x') ( -- f)

: REVISE  ( 87: x' --  s[L] -- s')
          FDUP  F(X)    DUP  SAME.SIGN?  ( 87: x' --) ( --s[L] f)
          IF   F(L) G!  XL FDUP  G!  OLDX G!
          ELSE F(R) G!  XR FDUP  G!  OLDX G!  THEN ;

: DEBUG   CR  F(L) G@ F.  XL  G@ F.  F(R)  G@ F.  XR  G@ F.  ;

: )FALSI  ( 87: PRECISION XL XR -- root)
      INITIALIZE   ( -- s[L])
      BEGIN   X'  APART
      WHILE   REVISE  ( DEBUG )  REPEAT   DROP ;

\ Usage:  USE( fn.name % 1.E-6  F=0  % 3.0  )FALSI

\ BEHEAD'' PRECISION REVISE
\ ------------------------------------------------ end regula falsi

DVARIABLE xi   0.0  xi  D!
: P[XE(-X)]-xi   ( 87: X -- 1-[1+X]*e**-X )
    FDUP  FEXP  FSWAP    F=1 F+  F\  F=1  FR-  xi R32@  F-  ;

: INV.P       ( 87: xi -- X[xi] )  xi R32!
     CFA'  P[XE(-X)]-xi  <F> !   % 1.E-6  % 0.0 % 10.  )FALSI ;

CFA' INV.P IS inv.p
6 DISTRIBUTION:  GAMMA1                      \ 64=2**6 bins

CREATE BINS  ' GAMMA1 @  2*  ALLOT           \ array to hold results
: INIT.BINS  BINS  ' GAMMA1 @  2*  0 FILL  ;       \ initialize

' GAMMA1 @  S->F  % 10. F/  FCONSTANT  scale
: TEST   INIT.BINS
         ' GAMMA1  START.RANDOM
         0 DO  GAMMA1    scale  F*  F->S
                    ' GAMMA1  @  MIN   0 MAX   2*  BINS +  1+!
         LOOP  ;

: READOUT  ' GAMMA1 @  0 DO  CR  BINS I 2*  +  @
                             I S->F  scale F/  F.  .  LOOP  ;




