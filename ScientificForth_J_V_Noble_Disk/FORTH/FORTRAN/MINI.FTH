\ Mini expression parser
\ version of 8:54:19  7/12/1990
\ compiles to 702 bytes
TASK MINI



: inc()   ( level char -- level' )   DUP   ASCII ( =  ABS
          SWAP  ASCII )  =  +   +   ;

: *or/?  DUP  42  =   SWAP  47 =  OR  ; ( char -- f)

: >term
       OVER   0  -ROT   1+  SWAP
       DO   I C@  UNDER  inc()
            SWAP  *or/?   OVER  0=   AND
            IF  DROP  I  LEAVE   THEN    LOOP
       UNDER  >  AND  ;

: .simple   ( $end $beg -- )   DUPC@  ASCII - =  DUP>R  -
        DDUP   UNDER   -  1+  TYPE  DDROP  SPACE
        R>  IF  ."  NEGATE "  THEN  ;

: +or-?   ( char -- f)  DUP  43  =   SWAP  45 =  OR  ;


: lead-   ( $end $beg -- token $end $beg+1 | $end $beg)
          DUPC@   ASCII -  =  IF  ASCII $  -ROT  1+  THEN  ;

: id?    -1   ROT  1+  ROT
          DO   I C@   DUP>R  *or/?   R@  +or-?  OR   \ *, /, + or - ?
               R@  40  =   R>  41 =  OR    OR        \ or ( or ) ?
               IF  NOT  LEAVE  THEN   LOOP ;  ( $end $beg -- f)

: >expr     DUPC@  45 =  -
        OVER   0  -ROT   1+  SWAP
        DO   I C@  UNDER  inc()
             SWAP  +or-?   OVER  0=  AND
             IF   DROP  I    LEAVE   THEN    LOOP
        UNDER  >  AND  ;

: break.at   ( $end $beg >op -- token $end >op+1 >op-1 $beg)
       SWAP  >R   DUPC@  -ROT  1+  DUP   2-  R>  ;

0 VAR DEPTH0

: expose    DUPC@  ASCII ( =   IF  1+  SWAP  1-  SWAP  THEN  ;

: .token?   ( char -- )
            BEGIN   DEPTH DEPTH0  >   OVER  50 U<  AND
            WHILE   DUP  ASCII $ =
                    IF    DROP   ."  NEGATE "
                    ELSE  SPACE EMIT  SPACE  THEN
            REPEAT  ;

: expression   ( $end $beg -- )
      DDUP  id?         IF  .simple  .token?            ELSE
      DDUP  >expr  DUP  IF  break.at  RECURSE           ELSE  DROP
      DDUP  >term  DUP  IF  break.at  RECURSE           ELSE  DROP
                            lead-  expose   RECURSE
                        THEN  THEN  THEN
      .token?
      DEPTH DEPTH0  >   IF  RECURSE  THEN  ;

: parse   ( $adr -- )   DEPTH  1-  IS DEPTH0
          COUNT  OVER  1-  +  SWAP   ( -- $end $beg)
          expression  ;

