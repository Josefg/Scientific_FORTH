CR .( Complex Fast Fourier Transform )
CR .( COPYRIGHT 1989, JULIAN V. NOBLE, ALL RIGHTS RESERVED. )

CR ." Usage: XDATA{ N DIRECT ( INVERSE ) }FFT "

TASK FFT

FIND  C+        0= ?( FLOAD COMPLEX )
FIND  1ARRAY    0= ?( FLOAD MATRIX.HSF )
FIND  FILL<FILE 0= ?( FLOAD FILEIO.FTH )
FIND >TRIG      0= ?( FLOAD TRIG )   DECIMAL

\ ================================================================
\                    auxiliary words
\ ----------------------------------------------------------------
    CODE  SHR  BX 1 SHR.  END-CODE
    : LG2  ( n -- lg2[n])   0 SWAP     ( -- 0 n)  SHR
        BEGIN   ?DUP 0>  WHILE   SHR   SWAP 1+  SWAP  REPEAT  ;


    0 VAR DIRECTION?
    : DIRECT  0 IS DIRECTION? ;   : INVERSE  -1 IS DIRECTION? ;

    0 VAR N.BITS   0 VAR N   0 VAR MMAX   0 VAR f{
    : INITIALIZE   IS N   IS f{   1 IS MMAX  N LG2  IS N.BITS ;

    : C/N    N S->F  C/F  ;
    : NORMALIZE    DIRECTION?
              IF   C/N  CPSWAP  C/N   CPSWAP   THEN ;
\ ============================================ end auxiliary words ;

\ ================================================================
\                    key bit-reveral routine!
\ ----------------------------------------------------------------
    0 VAR I.R
    : B.R  ( n -- n')             \ reverses order of bits
         0  SWAP   ( -- 0 n )     \ set up stack
         N.BITS 0 DO  DUP  1 AND  \ pick out 1's bit
                      ROT  2* +   \ double sum and add 1's bit
                      SWAP  2/    \ n -> n/2
         LOOP  DROP ;

    : BIT.REVERSE  0 IS I.R
         N 0  DO   I B.R   IS  I.R
           I.R  I  < NOT   ( I.R >=I ?)
           IF  f{ I.R } G@L  f{ I }  G@L    NORMALIZE
               f{ I.R } G!L  f{ I }  G!L  THEN
         LOOP ;
\ =============================================== end bit-reversal ;

\ ================================================================
\                    begin coding the algorithm proper
\ ----------------------------------------------------------------
CODE C-+   2 FLD.   1 FXCH.  3 FSUBR".  1 FADDP.  1 FXCH.
           3 FLD.   1 FXCH.  4 FSUBR".  1 FADDP.  1 FXCH.  END-CODE
    ( :: w z -- w-z  w+z)

    : THETA   F=PI  MMAX  S->F  F/  DIRECTION?
              IF  FNEGATE  THEN ;

    CREATE  WP  16  ALLOT OKLW
    : INIT.TRIG   FINIT  THETA  EXP(I*PHI)  WP  DCP!  C=1 ;

    : NEW.W   ( :: w -- w')  WP DCP@   C* ;

\ : DEBUG  CR  MMAX .  I' .  I.R  .  .FS  ;
    0 VAR ISTEP
\   : DO.INNER.LOOP   DO   MMAX I +  IS I.R
\                       CPDUP  f{ I.R } G@L  C*   f{ I } G@
\                       CPSWAP  C-+   DEBUG  f{ I } G!L  f{ I.R } G!
\                     ISTEP +LOOP ;
: CP.LP   DO  DDUP  MMAX I +  8* OVER +   SWAP I 8* +  CPDUP
     3 PICK 3 PICK  CP@L  C*  3 PICK SWAP  DDUP  CP@L  CPSWAP  C-+  CP!L
     CP!L  ISTEP +LOOP  ;
: DO.INNER.LOOP  f{ 0 } DROP DSWAP  CP.LP DDROP ;
: }FFT   INITIALIZE  FINIT  BIT.REVERSE
         BEGIN
              N MMAX >
         WHILE
              INIT.TRIG  MMAX  2* IS ISTEP
              MMAX 0 DO
                   N I  DO.INNER.LOOP   NEW.W
              LOOP
              ISTEP  IS  MMAX
         REPEAT  CPDROP  ;

: POWER  0 DO  f{ I } G@L  CABS  CR  I .  F.  LOOP ;
\                    exhibit power spectrum of FFT
\ =============================================== end of fft code

\ ================================================================
\                    an example
\ ----------------------------------------------------------------
64 LONG COMPLEX 1ARRAY A{

: INIT.A  A{ $" IBM.EX" OPEN-INPUT FILL<FILE  CLOSE-INPUT  ;
INIT.A
\ ================================================ end of example ;
