TASK MCINT
    0 VAR (AS)
    : ->  -1 IS (AS) ;  \ usage: -> A  puts a fp # into A
    : R32,   HERE-L  4 ALLOT  R32! ;
    : FVAR   CREATE  F=0  R32,  DOES>
         (AS)  IF  R32!  ELSE  R32@  THEN   0 IS (AS) ;
    BEHEAD' (AS)

    VARIABLE <F>
    : use{  [COMPILE] '  CFA   <F> ! ;
    : F(X)  <F>  EXECUTE@ ;
    BEHEAD' <F>

    FVAR A  FVAR B-A  FVAR E
    : from  -> A ;  
    : to   A  F-   -> B-A ;
    : error  -> E ;

    FVAR  Var.F   FVAR  Sum.Fsq   FVAR  Av.F

    : NEW.X   RANDOM  B-A F*  A F+  ;
    : Do.Average  ( n -- n+1 :: f -- f)
         FDUP   DUP  S->F   ( -- n :: -- f f n)
         Av.F     F*   F+   ( -- n :: -- f f+n*Av.F)
         1+  DUP  S->F  F/  ( -- n+1 :: -- f [f+n*Av.F]/[n+1])
         -> Av.F ;          \ update Av.F

    : Do.Variance  ( n -- n :: f -- )
         F**2  Sum.Fsq  F+    \ Sum.Fsq = Sum.Fsq + f^2
         FDUP  -> Sum.Fsq     \ update Sum.Fsq
         DUP   S->F  FUNDER   ( -- n :: n sum[f^2] n )
         F/  Av.F  F**2  F-  FR/   \ Var(f)=(Av(f^2)-(Av.F)^2)/n
         ->  Var.F  ;         \ update Var.F

    : INITIALIZE   FINIT
                      F=0  -> Var.F
                      F=0  -> Av.F
                      F=0  -> Sum.Fsq
         0  5 0 DO  NEW.X  F(X)  Do.Average  Do.Variance  LOOP ;

    : Not.Converged?    Var.F  FSQRT  B-A F*  E  F>  ;

    : DEBUG   DUP 10 MOD  
         0= IF  CR  DUP .  Av.F F.  Var.F F.   THEN ;
    : }MC.INTEGRAL           INITIALIZE
         BEGIN     DEBUG  Not.Converged?
         WHILE     NEW.X  F(X)  Do.Average  Do.Variance
         REPEAT    DROP   Av.F  B-A  F*  ;
\    BEHEAD'' A E
CR CR
.( use{ FN.NAME % L.lim from  % U.lim to  % ERR error }MC.INTEGRAL )
CR
