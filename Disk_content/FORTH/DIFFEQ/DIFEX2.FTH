\ Example f(x,t)

: FNB  ( FNEGATE) FEXP   F@  F**2   F* ;  ( a[t] -- :: x -- t^2*exp[-x])

\ set up initial cond'x

F=0 T F!   % 0.01 H F!    T Y 4 CMOVE     % 1.43 TMAX F!

( H F@  2 S->F  F/ <H/2> F! )     FINIT

( % 1.0E-6  PRECISION  F!)

5 #PLACES !

$" RUNGE.OUT" MAKE-OUTPUT
' FNB  RUNGE
CLOSE-OUTPUT

DOS" TYPE RUNGE.OUT"
