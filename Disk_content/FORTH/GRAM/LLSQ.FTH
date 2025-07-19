\ LLSQ.FTH
CR .( Gram polynomial linear least-squares fitting) CR 
CR .( COPYRIGHT  1988   JULIAN V. NOBLE)
CR .(                   105 POWHATAN CIRCLE)
CR .(                   CHARLOTTESVILLE, VA 22901) CR
CR .(                   FOR NON-COMMERCIAL USE ONLY) CR

CR .( Usage:  say FLOAD LLSQ.FTH)
CR .( then say: )
CR .(       1 SEGMENT SUPERSEG )
CR .(       IN SUPERSEG 100 LONG REAL 1ARRAY XDATA{ )
CR .(       IN SUPERSEG 100 LONG REAL 1ARRAY YDATA{ )
CR .(       IN SUPERSEG 100 LONG REAL 1ARRAY SIGMA{ )
CR .(       IN SUPERSEG 21  LONG REAL*8 1ARRAY A{ )
CR .(       IN SUPERSEG 21  LONG REAL*8 1ARRAY B{ )
CR .(       IN SUPERSEG 21  LONG REAL*8 1ARRAY C{ )
CR .(       IN SUPERSEG 100 LONG BY 21 WIDE REAL*8 2ARRAY G{{ ) 
CR .(       G{{ D.LEN  MAKE-KEY  D.I   D.I IS {D.I} ) CR
CR .( then say:   XDATA{ YDATA{ SIGMA{ GET.DATA  )
CR .(             answer questions and perform needed actions ) CR
CR .( then say:   A{ B{ C{ G{{ Max.Order }FIT )
DECIMAL
TASK LLSQ
FIND 1ARRAY 0= ?( FLOAD MATRIX.FTH )

\ ===============================================================
\              input words
\ ---------------------------------------------------------------
: >$  ( $adr --) IN$  DUPC@ 1+  ROT SWAP CMOVE ; 
\ put input line into string at $adr

: INPUT.MS   CR  ." The data file format must be:"
             CR  20 SPACES ." M.pts <cr>"
             CR  20 SPACES ." x{0} f{0} sigma{0} <cr>"
             CR  20 SPACES ." x{1} f{1} sigma{1} <cr>"
             CR  20 SPACES ." ששששששששששששששששששששששש"
             CR  20 SPACES ." x{M-1} f{M-1} sigma{M-1} <cr>"  ;
: LAST.MS   CR ." to run, say:   A{ B{ C{ G{{ Max.Order }FIT " ;
             
0 VAR M   0 VAR x{  0 VAR y{  0 VAR w{
0 VAR a{  0 VAR b{  0 VAR c{  0 VAR g{{
CREATE DataFileName$  50 ALLOT OKLW

: GET-F#  G$  $->F ;
: GET.DATA   INPUT.MS  IS w{  IS y{  IS x{
   CR  ." Is your file in this format (Y/N)?"  ?YN
   0= ABORT" Adjust your data file and start over."
   CR  ." What is the name of the data file ( d:[\path][\]file.ext)"
   DataFileName$  >$
   DataFileName$  OPEN-INPUT  <FILE  G# NR  IS M 
   <FILE
   M 0 DO  GET-F#  x{ I 0}  G!
           GET-F#  y{ I 0}  G!
           GET-F#  F**2  1/F  w{ I 0}  G!
   NR  LOOP  CLOSE-INPUT  LAST.MS ;
\ ===============================================================             

\ ===============================================================
\         CODEed words to simplify fstack management
\ ---------------------------------------------------------------
CODE G(N+1)  4 FMUL.  FCHS.  2 FLD.  6 FSUB.  2 FMUL.  1 FADDP.
             DX DS MOV.  DS POP.   R64  DS: [BX] FST.
             DS DX MOV.  BX POP.   END-CODE
( seg off --  :: s a b w x g[n] g[n-1] -- s a b w x g[n] g[n+1] )

CODE  B(N+1)  2 FXCH.  2 FMUL.  3 FMUL.  1 FXCH.  1 FMUL.
      DX DS MOV.  DS POP.  R64  DS: [BX]  FADD.  DS: [BX] FSTP.
      DS DX MOV.  BX POP.  END-CODE
( seg off --  :: s a b w x g[n] g[n+1] -- s a b w g[n+1] wxg[n+1] )

CODE  A(N+1)  1 FMUL.  DX DS MOV.  DS POP.  R64  DS: [BX] FADD.
              DS: [BX] FSTP.    DS DX MOV.  BX POP.  END-CODE
( seg off --  :: s a b w g[n+1] wxg[n+1] -- s a b w g[n+1] )

CODE C(N+1)  1 FXCH. 2 FMUL".  2 FMUL.  2 FXCH.  1 FMULP.
     DX DS MOV.  DS POP.  R64  DS: [BX] FADD.  DS: [BX] FSTP.
     DS DX MOV.  BX POP.  3 FADDP.  END-CODE
( seg off --   :: s a b w g[n+1] f -- s=s+wg[n+1]**2  a  b )
\ ===============================================================

\ ===============================================================
\            Gram polynomial coding
\ ---------------------------------------------------------------
\ Usage:      A{ B{ C{ G{{ Nmax }FIT
REAL*8 SCALAR  DELTA    0 VAR Nmax

: FIRST.AB's   F=0 a{ 0 0} G!    F=0 b{ 0 0} G!  ;

: INIT.DELTA   ( :: -- g{{ 1 I }} )    F=0  F=0
     M 0 DO  w{ I 0} G@  y{ I 0} G@   F**2  FOVER  F* 
             ( ::s s' -- s s' w wf^2)
             FROT  F+   F-ROT  F+  FSWAP ( :: -- s=s+w  s'=s'+wf^2 )
     LOOP    DELTA G!  FSQRT  1/F  ;

: FIRST.G's  ( :: g{{ 1 I }} -- g{{ 1 I }} ) 
     M 0 DO  F=0  g{{ 0 I }} G!  FDUP  g{{ 1 I }} G!  LOOP  ;

: SECOND.AB's   ( :: g{{ 1 I }} -- g{{ 1 I }} )
     F=0 b{ 1 0}  G!   FDUP  F**2
     F=0  M 0 DO  w{ I 0} G@ x{ I 0} G@  F*  F+  LOOP  
     F*  a{ 1 0} G! ;

: FIRST.&.SECOND.C's  ( :: g{{ 1 I }} -- )
     F=0 c{ 0 0} G!  F=0
     M 0 DO  w{ I 0} G@  y{ I 0} G@  F*  F+   LOOP
     F*  c{ 1 0} G! ;

: INITIALIZE   FINIT   IS Nmax  IS g{{  IS c{ IS b{  IS a{
     FIRST.AB's   INIT.DELTA   FIRST.G's  SECOND.AB's
     FIRST.&.SECOND.C's  ;

0 VAR N   0 VAR N+1
: inc.N   N+1  DUP  IS N   1+  IS N+1 ;

: DISPOSE   >R  DDUP  R@ G@   R> ;  \ just a convenient word
: inc.OFF   #BYTES DROP + ;  \ increment the offset by 1 item

: R64.ZERO.L   ( seg off --)  DDUP  F=0 R64!L  ( R32!L ) ;  

: START.Next.G   ( -- [c{n+1}] [a{n+1}] [b{n+1}]  :: -- s=0 a{n} b{n} )
        FINIT   F=0   c{ N+1 0} DROP  R64.ZERO.L
        a{ N 0} DISPOSE   inc.OFF  R64.ZERO.L
        b{ N 0} DISPOSE   inc.OFF  R64.ZERO.L  ;

: SET.FSTACK   
    w{ I' 0} G@  x{ I' 0} G@   g{{ N I' }} G@  g{{ N 1- I' }} G@   ;

: }@*!  ( adr n --  :: x -- x)  FDUP  0}  DISPOSE  F*  G! ;
\ just another convenient word

: NORMALIZE    ( :: sum -- )  
      1/F  a{ N+1 }@*!   FSQRT   b{ N+1 }@*!   c{ N+1 }@*!
      M 0 DO  FDUP  g{{ N+1 I }}  DISPOSE   F*  G!  LOOP   FDROP  ;

CODE 6DUP  OPT" 6 PICK  6 PICK  6 PICK  6 PICK  6 PICK  6 PICK " END-CODE
CODE 6DROP OPT" DDROP DDROP DDROP " END-CODE

: Next.G   START.Next.G 
      M 0 DO  6DUP    SET.FSTACK
              g{{ N+1 I }} DROP   G(N+1)   B(N+1)   A(N+1)
              y{ I 0} G@   C(N+1)
      LOOP    6DROP   FDROP FDROP   NORMALIZE  ;

: New.DELTA   ( :: -- old.delta new.delta)
      DELTA G@  FDUP   c{ N 0} G@  F**2  F-   FDUP  DELTA G! ;

: NOT.ENUF.G's?   New.DELTA    M N+1 -   S->F   FDUP  F=1 F- 
            ( CR  .FS  ."  NEXT ITERATION?" ?YN  0= IF ABORT THEN )
              ( :: -- d d' m-n-1  m-n-2 )  
              FROT  F\   F-ROT   F/   ( :: -- d'/[m-n-2]  d/[m-n-1] )
              FOVER  F0>  IF  F<=  ELSE  FDROP FDROP 0 THEN  ;


: }FIT  ( X{ Y{ S{ Nmax A{ B{ C{ G{{ -- )
        INITIALIZE    1 IS N   2 IS N+1
        BEGIN      NOT.ENUF.G's?     N Nmax <  AND 
        WHILE      Next.G   inc.N
        REPEAT  FINIT ;
\ ============================================== end of code ;

: RECONSTRUCT  M 0  DO  CR  x{ I 0} G@ F.  y{ I 0} G@ F. 
                 F=0  N+1 1+  1  DO 
                        c{ I 0} G@  g{{ I J }} G@  F* F+ 
                      LOOP   F. 
               LOOP    ;
\ exhibit the reconstructed values y'{I} at the points x{I} ;
