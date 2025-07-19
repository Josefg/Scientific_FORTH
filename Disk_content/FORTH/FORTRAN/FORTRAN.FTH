\ FORmula TRANslator
\ version of 7/17/93
TASK FORTRAN

SYNONYM IABS ABS             \ hook for old integer ABS function
\ ================================================ FINITE STATE MACHINE
: WIDE   0 ;
: FSM:   ( width 0 -- )   CREATE   ,  ,   ]   DOES>   ( col# -- )
               UNDER  D@        ( -- adr col# width state )
               *  +   1+  4*    ( -- adr offset )
               OVER +           ( -- adr adr'=adr+offset )
               UNDER  D>R  EXECUTE@    ( -- adr adr')   \ do action
               DR>  2+   EXECUTE@    ( -- adr state')
               SWAP !   ;       \ update state
\ IMPORTANT NOTE:
\ This version of FSM updates the state ***after*** the action.

0 CONSTANT >0   3 CONSTANT >3   6 CONSTANT >6
1 CONSTANT >1   4 CONSTANT >4   7 CONSTANT >7
2 CONSTANT >2   5 CONSTANT >5

\ ============================================ END FINITE STATE MACHINE

\ ========================================= Automatic conversion tables
\ CODE DOTAB  AX BX MOV.  BX 2 +[DI] LEA. BX AX ADD.
\             BL [BX] MOV.  BH BH SUB.  END-CODE
\ CODE DOTAB <% 89 D8 8D 5D 02 01 C3 %> END-CODE

: TAB:   ( #bytes -- )
           CREATE   HERE  OVER  ALLOT   SWAP  0 FILL  DOES>  +  C@  ;

\ ;' DOTAB
\           ( n tab[0] -- n')

: install      ( col# adr char.n char.1 -- )   \ fast fill
               SWAP 1+ SWAP   DO  DDUP I +  C!  LOOP  DDROP ;
\ ===================================== end automatic conversion tables

\ ===================================================== STRING HANDLING
HEX
FIND $!  0= ?( : $!   OVER C@ 1+ CMOVE ; )   ( srcadr  dstadr  --  )
: $+   ( adr$1 adr$2  -- pad )
       DUPC@ >R 1+  OVER C@ PAD + 1+      R@  <CMOVE
       PAD $!    R> PAD C@ +   0 MAX FF MIN    PAD C!  PAD ;
DECIMAL
: MAKE$   ( $end $beg -- PAD)  UNDER  -  1+  DUP>R  PAD C!
          PAD 1+  R>  CMOVE  PAD  ;
: expr.   ( $end $beg --)  UNDER  1-  -  TYPE  ;
\ ================================================= END STRING HANDLING

\ $STACK DEFINITIONS ====================================================
\ This $stack is 6 bytes wide, and keeps 3 items per cell:
\      token $end $beg
\      (the last 2 are D@ed and D!ed so their order is immaterial)

CREATE E/S   16  6 *  2+  ALLOT   OKLW
: E/S.init   -6  E/S  !  ;   E/S.init
: $PUSH   ( $end $beg token -- )  6  E/S  +!      \ inc ptr
          E/S @  96 > IF  CRT ." $stack too deep!"  ABORT THEN  \ safety check
          E/S DUP@  + 2+  DUP>R  !  R>  2+  D!  ;  \ put away data

: $POP    ( -- $end $beg token)    E/S  DUP@
          DUP  -6 <=
          IF CRT ." Can't pop empty $stack!"  ABORT THEN  \ safety check
          +  2+                ( -- adr[TOS] )
          DUP>R  2+  D@  R> @  ( -- $end $beg token)
          -6  E/S  +! ;        \ dec ptr

CREATE  E/S$  256  ALLOT  OKLW
\ ============================================== END $STACK DEFINITIONS ==

\ MNEMONICS  FOR FLOATING-POINT OPERATIONS ===============================
CFA' NOP CONSTANT .NOP          CFA' GSQRT  CONSTANT .FSQRT
CFA' FS> CONSTANT .FS>          CFA' GEXP   CONSTANT .FEXP
CFA' >FS CONSTANT .>FS          CFA' GLN    CONSTANT .FLN
                                CFA' G^     CONSTANT .F**
CFA' GS  CONSTANT .SINGLE       CFA' GSIN   CONSTANT .FSIN
CFA' GD  CONSTANT .DOUBLE       CFA' GCOS   CONSTANT .FCOS
CFA' GXX CONSTANT .CMPLX        CFA' GTAN   CONSTANT .FTAN
                                CFA' GATAN  CONSTANT .FATAN
CFA' G+  CONSTANT .F+           CFA' GASIN  CONSTANT .FASIN
CFA' G*  CONSTANT .F*           CFA' GACOS  CONSTANT .FACOS
                                CFA' GSINH  CONSTANT .FSINH
CFA' G/  CONSTANT .F/           CFA' GCOSH  CONSTANT .FCOSH
                                CFA' GTANH  CONSTANT .FTANH
CFA' G2* CONSTANT .F2*          CFA' GASINH CONSTANT .FASINH
CFA' G2/ CONSTANT .F2/          CFA' GACOSH CONSTANT .FACOSH
                                CFA' GATANH CONSTANT .FATANH
CFA' GNEGATE  CONSTANT .FNEGATE
CFA' GABS     CONSTANT .FABS

: op.     ( token -- )  \ OUTPUT MNEMONICS AS TEXT
         DUP   .NOP =  IF   DROP   ELSE   .WORD   THEN   2 SPACES  ;

\ Ex: .NOP  op.
\     .FS>  op.
\ ============================== END MNEMONICS  FOR FLOATING-POINT OPERATIONS

\ DEBUGGING CODE =========================================================
0 VAR DEBUG?
: DEBUG-ON   -1 IS DEBUG?  ;  : DEBUG-OFF  0 IS DEBUG?  ;
: .STACK   \ dump the $stack
      DEBUG?  IF   E/S  @  DUP  -6 <=  IF CRT ."  $STACK EMPTY" ABORT THEN
                   CR  ." expressions"  42 CTAB  ." operators"
                   CR  ." -----------"  42 CTAB  ." ---------"
                   6 +  0  DO   E/S 2+  I  +   DUP  2+  D@
                                CR  expr.            \ emit expression
                                42 CTAB  @   .WORD   \ emit operator
                           6 +LOOP
              THEN  CR  ;
BEHEAD' DEBUG?
: $ends   ( $adr -- $end $beg)  COUNT   DUP>R  0>  OVER  +  R>  +  SWAP  ;
\ ================================================= END DEBUGGING CODE ===

\ ================================================= data structures ======
CREATE  NULL$  1 C, 32 C,  OKLW
NULL$  COUNT  OVER  1-  +  SWAP   DVAR NULL
\ ================================================= end data structures ==
\ ========================================================== SEEK WORDS ==
0 VAR ()level
: +level   AT ()level  1+!  ;
: -level   AT ()level  1-!  ;
: %err     CRT ." Unbalanced parentheses!"    ABORT ;
: >0?  ()level  DUP  0>  IF  %err  THEN
       0= NOT IABS ;   \ 1 if ()level <>0, 0 if =0

4 WIDE FSM:  (%)
\ input:     |  other  |    )     |   (       |   %    |
\ state      -------------------------------------------
  ( 0 )       NEXT >0   -level >1  %err   >0   NEXT >2
  ( 1 )       NEXT >1   -level >1  +level >0?  NEXT >1   ;

128 TAB: [%]
1 ' [%]  ASCII )  +  C!
2 ' [%]  ASCII (  +  C!

: >%       ( $end $beg -- adr | 0 )      \ find exposed %
           0 IS ()level   ' (%)  0!       \ initialize
           SWAP   ( -- $beg $end)
           BEGIN  DUPC@  [%]  (%)
                  DDUP   <                \ $beg < $end ?
                  ' (%) @  2 <>  AND      \ not done?
           WHILE  1-  REPEAT              ( -- $beg $end')
           UNDER  <>  AND  ;

' [%]  ASCII *  +  CONSTANT '*
' [%]  ASCII /  +  CONSTANT '/
' [%]  ASCII ^  +  CONSTANT '^

: >*/    [%" 3 '*  C!
             3 '/  C!
             0 '^  C! " %]  >%  ;

: >^     [%" 0 '*  C!
             0 '/  C!
             3 '^  C! " %]  >%  ;
\ Note:	the notation [%" words " %] appearing above means "optimize these
\	words into in-line code in a word-body, using HS/Forth's recursive-
\	descent optimizer". To convert to another system, just edit out the
\	non-standard [%" the " and the %] . 

0 VAR id.len
: +id.len   AT id.len  1+!  ;
: >1?    id.len  7 <  DUP  1 AND  SWAP  NOT  2 AND +  ;
3 WIDE FSM: (id)
\ input:     |  other  |  letter   |    digit    |
\ state      -------------------------------------
   ( 0 )       NEXT >2   +id.len >1    NEXT >2
   ( 1 )       NEXT >2   +id.len >1?   +id.len >1?  ;

128 TAB: [id]
1 ' [id]  ASCII Z  ASCII A  install
1 ' [id]  ASCII z  ASCII a  install
2 ' [id]  ASCII 9  ASCII 0  install

: <id>   ( $end $beg -- f)   \ f = -1 for id, -2 for null, 0 else
         DDUP  NULL D=  IF  DDROP  -2  EXIT  THEN
         0 IS id.len   ' (id) 0!      \ initialize
         BEGIN   DUPC@  [id]  (id)
                 DDUP  >              \ $end > $beg ?
                 ' (id) @  2 <  AND   \ not terminated
         WHILE   1+  REPEAT   DDROP
         ' (id) @  1 =  ;             \ success?

: #err   CRT ." Not a correctly formed fp#" ABORT   ;
5 WIDE FSM: (fp#)
\ input:     |  other  |   dDeE    |   digit  |   &     |    dp    |
\ state:     -------------------------------------------------------
   ( 0 )       NEXT >7    NEXT >7     1+  >0    NEXT >7   1+     >1
   ( 1 )       NEXT >7    1+   >2     1+  >1    #err >7   #err   >7
   ( 2 )       NEXT >7    #err >7     1+  >4    1+   >3   #err   >7
   ( 3 )       NEXT >7    #err >7     1+  >4    #err >7   #err   >7
   ( 4 )       NEXT >7    #err >7     1+  >5    #err >7   #err   >7
   ( 5 )       NEXT >7    #err >7    #err >7    #err >7   #err   >7 ;

128 TAB: [fp#]
1 ' [fp#]  ASCII E  ASCII D  install
1 ' [fp#]  ASCII e  ASCII d  install
2 ' [fp#]  ASCII 9  ASCII 0  install
3 ' [fp#]  ASCII +  +  C!
3 ' [fp#]  ASCII -  +  C!
4 ' [fp#]  ASCII .  +  C!

: skip-  DUPC@  ASCII - =  -  ;
: <fp#>   ( $end $beg -- f)
         ' (fp#) 0!   skip-           \ initialize
         BEGIN   DUPC@  [fp#]  (fp#)
                 DDUP  <=             \ $end = $beg ?
                 ' (fp#) @  7 =  OR   \ terminated by error
         UNTIL   DDROP
         ' (fp#) @  7 <  ;            \ success?

FIND UCASE  0=
?( : UCASE    DUP  ASCII a  MAX  ASCII z MIN  OVER =  32 AND -  ; )

: fp#.    ( $end $beg --)                   \ send-FORTH a fp#
          ." 87>FS "                         \ . "push tos"
          ." % "  SWAP 1+  SWAP  0  -ROT    ( -- 0 $end+1 $beg)
          DO  I C@  UCASE  DUP  ASCII D =   \ single or double ?
              IF  DROP   ASCII E  DUP  I C!
              ELSE  DUP  ASCII E =  ROT  OR  SWAP  THEN
              EMIT
          LOOP  IF  ."  REAL*4"  ELSE   ."  REAL*8"  THEN
          ."  87type ! " ;                 \ tos type descriptor

0 VAR <f^f>
: <f>^<f>   ( $end $beg -- f )
            >^  DUP IS <f^f>  0=  NOT  ;
: f^f!      ( $end $beg --)
            SWAP  <f^f> 1+  .F**  $PUSH
            <f^f> 1-  SWAP  .NOP  $PUSH ;

: inc()   ( c --)  DUP  ASCII ( =  IABS    SWAP  ASCII ) =  +
                   AT  ()level  +!  ;
: (<expr>)    ( $end $beg -- f)   \ "enclosed" ?
       DUPC@  ASCII ( <>  IF  DDROP  0  EXIT THEN  \ first char <> "("
       1+    ( $end $beg+1 )
       1  IS  ()level                              \ initialize
       BEGIN    DDUP  >=   ()level  0>   AND
       WHILE    DUPC@   inc()
       1+  REPEAT     U<    ()level  0=  AND  ;

0 VAR <char>
: >>->    ( $end $beg char -- $end $beg')  \ find first instance of char
      IS <char>   1-
      BEGIN   1+  DUPC@  <char>  =  >R  DDUP  =  R>  OR  UNTIL  ;
BEHEAD' <char>

0 VAR <fn>
: <function>   ( $end $beg -- f )   \ "Is this a function before my eyes?"
    DUP>R  ASCII (  >>->            \ find "("
    DUP  IS  <fn>                   \ save pointer
    DUP  1-  R>                     ( $end $beg' $beg'-1 $beg)
    <id>  >R  (<expr>)  R>  AND  ;

\ ---- WORDS TO DEFINE AND USE FUNCTION LIBRARY -----------------------------

: DEFINED?    ( $adr -- cfa OR 0)
      CONTEXT @  <FIND>  IF  DROP  @E   ELSE   0  THEN ;
\ note: F83 "FIND" will work here.

: FUNC{  [COMPILE]  CFA'  ; IMMEDIATE
: }TION   STATE @  IF  COMPILE  EXECUTE  ELSE  EXECUTE  THEN ; IMMEDIATE

CFA' FUNC{  CONSTANT  .FUNC{                   \ to indicate a user
CFA' }TION  CONSTANT  .}TION                   \ function

\ ------ FUNCTION LIBRARY ---------------------------------------------------

: FUNC   ( .op --)  CREATE  ,    DOES>  @    NULL  ROT  $PUSH  ;

.FABS   FUNC  ABS       .SINGLE FUNC  SINGLE     .DOUBLE FUNC  DOUBLE
.CMPLX  FUNC  CMPLX
.FEXP   FUNC  EXP       .FSQRT  FUNC  SQRT       .FLN    FUNC  LOG
.FSIN   FUNC  SIN       .FCOS   FUNC  COS        .FTAN   FUNC  TAN
.FATAN  FUNC  ATN       .FASIN  FUNC  ASIN       .FACOS  FUNC  ACOS
.FSINH  FUNC  SINH      .FCOSH  FUNC  COSH       .FTANH  FUNC  TANH
.FASINH FUNC  ASINH     .FACOSH FUNC  ACOSH      .FATANH FUNC  ATANH

$" EXP"  DEFINED?  @  CONSTANT  LIB.ADR
\ this works because LIB.ADR is the same for all children of FUNC

: IN.LIB?   ( cfa -- f)  @   LIB.ADR  =  ;
\ ------------------------------------ END FUNCTION LIBRARY WORDS -----------

: >>->,    ( $end $beg -- $end $beg')   \ find first exposed ,
      0 IS ()level   1-
      BEGIN  1+  DUPC@  DUP  inc()  ASCII , =
             ()level  0= AND  >R
             DDUP =  R>  OR
      UNTIL  ;

: arg.list!   $POP  ( $end $beg token )  -ROT  DDUP  D>R
      >>->,   ( -- token $end $beg')  DDUP  =  \ no "," found
      IF   DDROP  DR>  ROT  $PUSH  EXIT   THEN
      1+   DUP  2-   DR>  PLUCK   ( -- token $end $beg'+1 $beg'-1 $beg)
      .NOP   $PUSH                \ push an arg
      ROT    $PUSH  RECURSE  ;    \ push remainder and recurse

: function!   ( $end $beg -- )
      <fn>  1-  SWAP   ( $end $beg -- $end $beg'-1 $beg)
      DDUP   MAKE$     ( $end $beg -- $end $beg'-1 $beg PAD)
      DEFINED?  DUP  0=  IF CRT ." Undefined function!" ABORT THEN
      DUP  IN.LIB?
      IF    EXECUTE   DDROP              \ in library
      ELSE  DROP                         \ user function
            .}TION  $PUSH                \ push <id> and }TION
            NULL .FUNC{  $PUSH           \ push FUNC{
      THEN  ( -- $end)   1-  <fn> 1+     \ expose arg.list
      .NOP  $PUSH   arg.list!  ;         \ push argument

: <f>%<trm>    ( $end $beg -- f )
     >*/  DUP IS <f^f>  0=  NOT  ;
: .%   <f^f> C@  ASCII * =  IF  .F*  ELSE  .F/  THEN  ;
: f%trm!      ( $end $beg --)
     SWAP  <f^f> 1+  .%    $PUSH
     <f^f> 1-  SWAP  .NOP  $PUSH ;

0 VAR '<expression>                \ forward reference VAR

\ ========================================================= PARSE WORDS
: empty?   E/S @  -6 = ;

: ?push     DUP  .NOP <>  IF  NULL  ROT  $PUSH  ELSE  DROP  THEN  ;

\ <factor> ->  <fp#> | <id> | <fac> ^ <fac> | <function> |  ( <expression> )
: <factor>  empty?  IF  EXIT  THEN                  \ done
            $POP    ( -- $end $beg token)
            -ROT  D>R  ( -- token)                  \ save $end $beg
            DR@   <fp#>  ( -- token f)              \ floating point # ?
                  IF   DR>  fp#.   op.  '<expression> EXECUTE
                  EXIT  THEN
            DR@   PLUCK  C@  ASCII -  =             \ leading - ?
                  IF   ?push
                       DR>  1+  .FNEGATE  $PUSH
                       '<expression>  EXECUTE       \ forward ref.
                  EXIT  THEN
            DR@   <id>   ( -- token f)   ?DUP  \ name ? -2 = null, -1 = id
                  IF  DR>  expr.                    \ emit id
                      -1 =  OVER  .}TION  <>  AND
                      IF ."  >FS "  THEN            \ push on FS if not fn
                      op.                           \ emit math op
                      '<expression>  EXECUTE        \ forward ref.
                  EXIT  THEN
            DR@   <f>^<f>  ( -- token f)            \ exponentiation ?
                  IF  ?push
                      DR>  f^f!  RECURSE   RECURSE
                  EXIT  THEN
            DR@   <function>  ( -- token f)         \ a function ?
                  IF   ?push
                      DR>  function!
                      '<expression>  EXECUTE        \ forward ref.
                  EXIT  THEN
            DR@   (<expr>)   ( -- token f)          \ expression in parens ?
                  IF  ?push
                      DR>  1+  SWAP  1-  SWAP       \ expose
                      .NOP  $PUSH                   \ <expr'n>  .nop
                      '<expression>  EXECUTE        \ forward ref.
                  ELSE  DRDROP  CRT ." Not a factor!"  ABORT
                  THEN ;

\ <term>   ->  <factor> | <factor> {*|/} <term>
: <term>    empty?  IF  EXIT  THEN       \ done
            $POP    ( -- $end $beg token)
            -ROT  D>R   ( -- token)
            DR@   <f>%<trm>   ( -- token f)
            IF  ?push
                DR>   f%trm!   RECURSE
            ELSE  DR>  ROT  $PUSH  <factor>  THEN  ;

4 WIDE FSM:  (+-)
\ input:     |  other  |    )     |   (       |   +-    |
\ state      ---------------------------------------------
  ( 0 )       NEXT >0   -level >1  %err   >0   NEXT >2
  ( 1 )       NEXT >1   -level >1  +level >0?  NEXT >1   ;

128 TAB: [+-]
1 ' [+-]  ASCII )  +  C!
2 ' [+-]  ASCII (  +  C!
3 ' [+-]  ASCII +  +  C!
3 ' [+-]  ASCII -  +  C!

: >+-      ( $end $beg -- adr | 0 )      \ find exposed + or -
           0 IS ()level   ' (+-)  0!      \ initialize
           SWAP   ( -- $beg $end)
           BEGIN  DUPC@  [+-]  (+-)
                  DDUP   <                \ $beg < $end ?
                  ' (+-) @  2 <>  AND     \ not done?
           WHILE  1-  REPEAT              ( -- $beg $end')
           UNDER  <>  AND  ;

: >digits   ( adr -- adr')   BEGIN  1+  DUPC@  [fp#] 2 <>  UNTIL  1- ;
: <dDeE     ( adr -- adr')   BEGIN  1-  DUPC@  [fp#] 1 <>  UNTIL  ;
: <digits   ( adr -- adr')   BEGIN  1-  DUPC@  [fp#] 2 <>  UNTIL  ;
: <~fp#>    ( adr -- f)   DUP  >digits  DDUP = -  ( -- adr $end)
            OVER  <dDeE   1+  <digits              ( -- adr $end adr')
            DUPC@  ASCII . = +       \ skip.dp
            1+ <digits  1+  ROT  OVER = +   ( -- $end $beg)
            <fp#>  NOT  ;

: <trm>&<expr>    ( $end $beg -- f )
     DDUP  >+-   DUP  IS <f^f>   ( -- $end $beg  adr | 0 )
     0= NOT  IF  <f^f>  <~fp#>  ( -- $end $beg f)  \ not in a fp# ?
                 IF DDROP  -1  EXIT  THEN          \ found
                 PLUCK   <f^f>  DDUP  <
                 IF  1- SWAP  RECURSE  EXIT   THEN
     THEN  DDROP  0  ;                             \ not found

: trm&expr!      ( $end $beg --)
     SWAP  <f^f> DUPC@ ASCII + = -  .F+   $PUSH
     <f^f> 1- SWAP  .NOP  $PUSH ;

\ <expression>    -> <term> | <term> & <expression>
: <expression>    empty?  IF  EXIT  THEN       \ done
            $POP    ( -- $end $beg token)
            -ROT  D>R   ( -- token)
            DR@   <trm>&<expr>   ( -- token f)
            IF  ?push
                DR>   trm&expr!  RECURSE
            ELSE  DR>  ROT  $PUSH  <term>  THEN  ;

CFA' <expression> IS '<expression>         \ fulfill forward reference

\ ===================================================== END PARSE WORDS

\ ========================================================= INPUT WORDS
: **A  CRT  ." Can't have 3 *'s in a row!"  ABORT  ;
: put^  ( adr -- )   32  OVER 1-  C!  ASCII ^ SWAP C! ;
2 WIDE FSM: (**->^)
\ input:    |  other  |    *     |
\ state:    ----------------------
  ( 0 )       DROP >0     DROP >1
  ( 1 )       DROP >0     put^ >2
  ( 2 )       DROP >0     **A  >2   ;

: **->^    ( $end $beg -- )  ' (**->^)  0!  SWAP 1+ SWAP
           DO  I  DUPC@   ASCII * = IABS   (**->^)   LOOP  ;

: skipBL   1- BEGIN 1+  DUPC@  BL = NOT >R DDUP = R> OR  UNTIL  PLUCK ;
: >BL      ( $end $beg -- $end adr)  BL  >>->  ;
: -BL  ( $end $beg -- $end' $beg)
       DUP>R  BEGIN   >BL  DDUP   ( $end adr $end adr)
                      skipBL      ( -- $end adr adr+n-1)
              DDUP  <  WHILE  OVER  DDUP  -      ( $end adr src dst #bl)
                    -ROT  D>R  -ROT              ( #bl $end adr)
                    DDUP - 1+  DR>  ROT   CMOVE  ( #bl $end adr)
                    -ROT  SWAP  -  SWAP
              REPEAT  DDROP  R>  ;


CREATE BUF 256 ALLOT OKLW
: >BUF        \ READ AN EXPRESSION FROM THE KEYBOARD
        BUF 255 0 FILL                   \ 0's in BUF
        BEGIN    BL AWORD                \ get input from wherever
                 BUF SWAP $+   BUF $!    \ add input to BUF
                 BUF COUNT + 1- C@       \ get last char
                 ASCII "  =              \ is last input char " ?
        UNTIL                            \ repeat
        BUF   $ends  DDUP  **->^  -BL    \ replace ** by ^
        -   BUF  C!     ;                \ update count

\ <assign>  ->   <id> = <expression>
: <assign>   $POP  -ROT             ( -- op $end $beg)
             DUP>R  ASCII =  >>->   ( -- op $end $beg')
             DDUP  >                \ "=" found
             IF  1+  DUP  2-  R>    ( -- op $end $beg'+1 $beg'-1 $beg)
                 DDUP  <id>  NOT IF CRT ." Incorrect assignment!" ABORT THEN
                 D>R   ROT  $PUSH   <expression>
                 DR>  expr.  .FS>  op.
             ELSE  DROP  R>  ROT  $PUSH    <expression>
             THEN ;

1000 SEGMENT FORTSEG         \ make a segment to hold output from F"  "

: F"->MEM     DEBUG-OFF                               \ turn off debugging
              FORTSEG @  0  1000 0 FILLL              \ initialize buffer
              FORTSEG  OPEN-MEM  >MEM  ;              \ output -> buffer

: MEM->    FORTSEG @  0  MLOAD                  \ load from FORTSEG
           CLOSE-MEM  ;

: F."    CR  E/S.init   >BUF   BUF $ends .NOP $PUSH  <assign>  ;
\ output the FORTH code

: F"    CR  E/S.init   >BUF
        F"->MEM                                   \ vector to FORTSEG
        BUF $ends .NOP $PUSH  <assign>            \ output the FORTH code
        CRT                                       \ return output to CRT
        MEM->   ;   IMMEDIATE                     \ EXECUTE or COMPILE

SYNONYM  REDO CLOSE-MEM
DEBUG-ON
