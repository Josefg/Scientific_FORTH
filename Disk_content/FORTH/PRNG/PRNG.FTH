CODE  I32@   <% 9B DB 07 5B 9B %> END-CODE
CODE  I32!   <% 9B DB 1F 5B 9B %> END-CODE

: FUNDER  FSWAP  FOVER  ;

DVARIABLE BIGDIV   21474.83647 BIGDIV D!   \ 2**31-1
DVARIABLE DIVIS    1277.73     DIVIS  D!
CREATE SEEDS 64 4* ALLOT OKLW

VARIABLE ISEED    ISEED 0!
: SEED  ISEED  @  SEEDS + ;
0.1   SEED   D!

VARIABLE M1   16807 M1 !
VARIABLE M2    2836 M2 !

: XDUP  FOVER FOVER ;

: RAND     ( :: -- seed) 
         SEED  DUP  I32@  DIVIS I32@   XDUP
         F/  FRNDINT  FUNDER
         F*   FROT   FR-   M1 I16@   F*
         FSWAP   M2 I16@   F*  F-  FDUP  I32! ;

\ : INIT-RAND  64 0 DO I 4*  ISEED !  RAND  FDROP  LOOP ;

: RANDOM    ( :: -- random#)
            RAND  BIGDIV I32@   ( :: -- seed 2**31-1)
            FSWAP  FDUP  F0<    ( -- f :: -- 2**31-1 seed)
            IF  FOVER  F+  THEN  F\  ;

: TEST  0.1  SEED D!  1000 0 DO  RANDOM  FDROP  LOOP  SEED D@ D. ;
\ TEST 522329230  ok

