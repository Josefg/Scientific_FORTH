
\ Integration by recursion -- an illustration of the concept
\ Usage:  USE( FN.NAME  from A to B error E type )INTEGRAL
\ Warning: FN.NAME  must not overflow the 8087 fstack --
\          use FS> and >FS to offload temps to the ifstack

CR .( COPYRIGHT 1989  JULIAN V. NOBLE )
CR .(                 105 POWHATAN CIRCLE )
CR .(                 CHARLOTTESVILLE, VA 22901 ) CR
CR .(                 FOR NON-COMMERCIAL USE ONLY ) CR

TASK INTEGRAL

    FIND >FS  0= ?( FLOAD IFSTACK.FTH)
    FIND CPDUP   0= ?( : CPDUP  FOVER  FOVER ; )
    FIND FUNDER 0= ?( : FUNDER  FSWAP  FOVER ;

    VARIABLE <F>
    : USE(   [COMPILE]  '   CFA  <F> ! ;
    : F(X)   <F>  EXECUTE@  ;
    BEHEAD'  <F>

    : from ;   : to  ;   : error ;

: GDUP   FS>F  DUP  DUP  2 AND  IF CPDUP  ELSE FDUP THEN  F>FS F>FS ;

FINIT
4 DCOMPLEX SCALARS F1 F2 F3 old.I
4 REAL*8 SCALARS A B C E
0 VAR type

    : )integral  ( :: A B F1 F2 -- int)
       G+  G2/   FS>F   G-   F>FS  G*  ;

    : SET.UP  ( :: A B F1 F2 E -- )
        E FS>  F2 FS>  F1 FS>   B FS>  A FS>
        A G@ B G@  F+  F2/   FDUP   C G!   F(X)  F3 G! ;

    : CONVERGED?   ( -- f  :: -- I4 I4-I2 )
         A >FS C >FS F1 >FS F3 >FS )integral
         C >FS B >FS F3 >FS F2 >FS )integral  G+  GDUP
         A >FS B >FS F1 >FS F2 >FS )integral  G-  GDUP
         GABS  FS>F  DROP  E G@   F<  ;

    : SET.STACK  ( :: -- A C F1 F3 E/2   C B F3 F2 E/2 )
          A >FS  C >FS   F1 >FS   F3 >FS  E DUP>R  G@ F2/  FDUP   R@ F>FS
          C >FS  B >FS   F3 >FS   F2 >FS   R>  F>FS  ;

    : STORE.I    % 3  REAL*8  F>FS  G/  G+   old.I >FS  G+   old.I  FS>  ;

    : ADAPT   ( :: A B F1 F2 E -- )
        TSTACK DUP@  <
        IF  SET.UP   CONVERGED?
            IF    STORE.I
            ELSE  T.DROP   T.DROP
                  SET.STACK  RECURSE  RECURSE   THEN
        THEN  ;

    : re-type  ( t' seg.off t -- )  DROP    2- !L ;

    : INITIALIZE  ( t --  87::  A B E --   :: -- A B F1 F2 E )
        IS type   type  F1 re-type  type  F2 re-type
        type  F3 re-type   type  old.I re-type
        type  1 AND  DUP  E re-type  DUP C re-type
        DUP  B re-type  A re-type
        E G!   B G!  A G!   type 2 AND  IF F=0 THEN  F=0   old.I  G!
        A G@   F(X)  F1 G!
        B G@   F(X)  F2 G!    FS.INIT
        A >FS   B >FS   F1 >FS   F2 >FS   E >FS  ;

    : )INTEGRAL  ( 87::  A B E -- :: I[A,B] )
        INITIALIZE  ADAPT  old.I  >FS ;




