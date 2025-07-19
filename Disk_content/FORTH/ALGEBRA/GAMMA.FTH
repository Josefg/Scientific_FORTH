\ GAMMA MATRIX ALGEBRA
\ version of 20:49:46  12/27/1991
\ ALGORITHM:

\   ( x/ denotes a string containing an even number of factors, e.g.
\         A/B/C/D/  )

\             Tr( a/b/x/) = a.b ( Tr( x/) )  - Tr( a/x/b'/)

\     say   PERP  a b   to force a.b = 0.

\     We turn on bit 7 (0-7) in first character of factor b, which we
\     denote as b', i.e. b' = b OR 128.

\    Next we can do

\             Trg5( a/b/c/d/x/) ð iTr( ^/d/x/)
\    where
\             ^.d ð [a,b,c,d]

\    and [a,b,c,d] is the fully antisymmetric (pseudoscalar) product
\    of 4 4-vectors.

\    Finally, we can do [ *a/ ð a/ + ma ]

\    Tr( *a/x/ ) ð ma Tr( x/ ) + Tr( x/ a/)

TASK GAMMA


\ ------------------------------------------------ FINITE STATE MACHINE
: WIDE   0 ;
: FSM:   ( width 0 -- )   CREATE   ,  ,   ]   DOES>   ( col# -- )
               UNDER  D@        ( -- adr col# width state )
               *  +   1+  4*    ( -- adr offset )
               OVER +           ( -- adr adr' )
               DUP@  SWAP 2+    ( -- adr [adr']  adr'+2 )
               EXECUTE@         ( -- adr [adr']  state')
               ROT !   EXECUTE   ;

0 CONSTANT >0   3 CONSTANT >3
1 CONSTANT >1   4 CONSTANT >4
2 CONSTANT >2   5 CONSTANT >5

\ -------------------------------------------- END FINITE STATE MACHINE

\ ----------------------------------------- Automatic conversion tables
\ CODE DOTAB  AX BX MOV.  BX 2 +[DI] LEA. BX AX ADD.
\             BL [BX] MOV.  BH BH SUB.  END-CODE
\ CODE DOTAB <% 89 D8 8D 5D 02 01 C3 %> END-CODE

: TAB:   ( #bytes -- )
           CREATE   HERE  OVER  ALLOT   SWAP  0 FILL  DOES>  +  C@  ;

\ ;' DOTAB
\           ( n tab[0] -- n')

: install      ( col# adr char.n char.1 -- )   \ fast fill
               SWAP 1+ SWAP   DO  DDUP I +  C!  LOOP  DDROP ;
\ ------------------------------------- end automatic conversion tables

\ ----------------------------------------------------- STRING HANDLING
HEX
FIND $! 0= ?((( : $!  ( sadr dadr -- )  OVER C@ 1+     CMOVE ; )))

FIND $+ 0= ?((( : $+        ( adr$1 adr$2  -- pad )     ( -- $adr a)
            DUPC@ >R 1+  OVER C@ PAD + 1+      R@  <CMOVE
            PAD $!       R> PAD C@ +   0 MAX FF MIN    PAD C! ; )))

: $=  ( $adr1 $adr2 -- f )   -1  -ROT
      COUNT  ROT  COUNT  ( -- $adr2+1 n2 $adr1+1 n1 )
      ROT DDUP  =
      IF  DROP  0  DO  DUPC@  7F AND  >R  1+
                       OVER  C@  7F  AND  R>  <>
                       IF   ROT  NOT  -ROT  LEAVE  THEN
                       SWAP  1+
                LOOP   DDROP
      ELSE  DDROP DDROP  NOT  THEN  ;

FIND $.ENDS 0= ?((( : $.ENDS   ( $adr -- $end $beg)
        COUNT  OVER  +  1-  SWAP  ; )))
DECIMAL

\ CODE  1+C!    [BX] BYTE-PTR INC.  BX POP.   END-CODE
CODE  1+C!  <% FE 07 5B %>  END-CODE

: -BL   ( $adr -- )         \ delete all blanks from $
        DUP>R    0 PAD C!
        COUNT OVER + SWAP
        DO   I  C@  DUP    32 <>
             IF     PAD COUNT  +  C!  PAD 1+C!
             ELSE   DROP   THEN
        LOOP   PAD  R>  $!  ;
\ ------------------------------------------------- END STRING HANDLING

\ --------------------------------------------------------- PARSE WORDS
0 VAR  star                       \ to indicate whether a * was 1st char
: star!   128 IS star  DROP  ;

0 VAR  #/
: +#/   AT #/  1+!  DROP  ;

: error   CR  -1  ABORT" Name -> letter ( letter | numeral )* "  ;

: +PAD   ( char -- )    star  OR            \ set bit 7
         PAD COUNT  +   C!  PAD  C@  1+  PAD  C!   0 IS star  ;

5 WIDE FSM: (factor)   ( char col# -- )
\ input |  other   | character |  numeral  |    /    |     *     |
\ state ----------------------------------------------------------
  ( 0 )  error  >0   +PAD   >2   error  >0   +#/   >5   star!  >1
  ( 1 )  error  >0   +PAD   >2   error  >1   +#/   >5   error  >1
  ( 2 )  error  >0   +PAD   >3   +PAD   >3   +#/   >5   error  >2
  ( 3 )  error  >0   +PAD   >4   +PAD   >4   +#/   >5   error  >3
  ( 4 )  error  >0   DROP   >4   DROP   >4   +#/   >5   error  >4  ;

\ Note: The notation *A means (A/ + M sub A)

128  TAB:   [factor]
1  ' [factor]  ASCII Z ASCII A  install
1  ' [factor]  ASCII z ASCII a  install
2  ' [factor]  ASCII 9 ASCII 0  install
3  ' [factor]  ASCII /   +  C!
4  ' [factor]  ASCII *   +  C!

: <factor>  ( adr -- adr')
            PAD 0!  ' (factor) 0!  0 IS star   \ initialize
            BEGIN  DUPC@   DUP   [factor]
                   (factor)  1+
            ' (factor)  @  5 =  UNTIL  ;

CREATE  factor{  20 4*  ALLOT  OKLW          \ up to 20 factors

: }   ( adr n -- adr + 4*n)  4*  +  ;        \ compute address

0 VAR N
0 VAR N1
CREATE BUF$  20 ALLOT  OKLW

: check.table  -1 IS N1
     N 0  DO  PAD factor{ I } $=  IF  I IS N1  LEAVE  THEN  LOOP ;

: +buf$   N N1 -1 = AND  N1 -1 > N1 AND  +
          PAD 1+ C@ 128 AND  OR
          BUF$  DUPC@ + 1+  C!  BUF$  1+C!  ;

: tokenize   ( $adr -- )                     \ decompose into factors
     factor{ 80  0 FILL                      \ initialize table
     COUNT   OVER +  ( -- $adr+1 $adr')      \ do addresses
     >R                                      \ $adr' = $adr+LEN($)+1
     0  BUF$  C!                             \ init. buffer
     0 IS N                                  \ init. N
     BEGIN  <factor>                         \ begin loop; get factor
            check.table                      \ no multiple entries
            +buf$                            \ create tokenized $
            N1 -1 =                          \ not in table?
            IF  PAD  factor{ N } $!          \ put in table
                AT  N  1+!                   \ inc. N
            THEN
     DUP R@  =  UNTIL                        \ end loop
     DROP  RDROP  ;                          \ clean up
\ ----------------------------------------------------- END PARSE WORDS

CREATE X$ 80 ALLOT OKLW
: get$  ASCII )   TEXT  PAD X$  $!  ;

CREATE  $stack   20 20 * 2+ ALLOT OKLW
: $push   ( $adr - -)  DUPC@  19 > ABORT" STRING TOO LONG!"
          $stack  DUP@
          DUP  19 >  ABORT" $stack too deep!"
          20 *  2+  +    $!
          $stack 1+!  ;

: $pop    ( $adr - -)  $stack  DUP@   1-  0  MAX  DDUP SWAP  !
          20 *  2+  +  SWAP  $!  ;

: 2factors   BUF$ C@  2 >
             IF   BUF$  COUNT  2-  >R  2+
                  X$ 1+  R@  <CMOVE   R>  X$  C!
             ELSE X$ 0!   THEN
             BUF$  1+  DUPC@  SWAP  1+  C@  ;

0 VAR sign

2 TAB:  [sign]
ASCII +  ' [sign]  C!
ASCII -  ' [sign]  1+  C!

: .sign      ( a - -)  64 AND  0> ABS  [sign]  sign AND  EMIT  ;

: A.B        63 AND          \ b' -> b
             OVER   .sign
             SWAP  63 AND
             DDUP  MAX  -ROT   MIN
             factor{ SWAP }  $.
             ASCII . EMIT
             factor{ SWAP }  $.
             -1 IS sign ;

: clean      ( $adr - -)  COUNT OVER + SWAP
             DO  I C@  63 AND  I C!  LOOP  ;

CREATE A$  2 ALLOT OKLW
: rearrange  ( a b - - )   X$  BUF$  $!
             64 OR                        ( - - a b')
             X$ COUNT +  C!   X$ 1+C!     \ x/ -> x/b'/
             64 XOR                       \ toggle sign of a
             1 A$  C!  A$  1+  C!         \ make A$
             A$   X$  $+                  \ PAD =  A$ + X$
             PAD $push    BUF$  clean  BUF$  $push  ;

: setup   X$  tokenize       \ form token$
          $stack  0!         \ init $stack
          0 IS sign          \ init sign flag
          BUF$  $push  ;

: $$.     ( $adr - -)  COUNT  OVER + SWAP    \ translate token$
          DUPC@  .sign   factor{ OVER C@ 63 AND } $. ." /"
          1+  DO  factor{ I C@ 63 AND }   $.
                  I C@  64 AND 0> IF  ." '"  THEN    ." /"
               LOOP ;

: .$stack  $stack 2+    $stack @  DUP  0=
           IF  DDROP   CR ." $stack empty"  EXIT  THEN
           0  DO  I  20 *  OVER +  CR $$.   LOOP  DROP  ;
0 VAR DEBUG
: parse   DEBUG  IF .$stack  THEN
          BUF$ $pop
          2factors           ( - - a b)
          DUP  64 AND  0>    \ b = b' ?
          IF   DDROP  EXIT  THEN
          DDUP  A.B
          X$ C@ 0>
          IF     rearrange
                 ." ("   0 IS sign
                 RECURSE  ." )"  -1 IS sign
                 RECURSE
          ELSE   DDROP
          THEN   ;

: TR(     get$               \ input $
          setup  ."  ="  CR
          parse  ;
