@LISTING = % 0.906179845938664 FCONSTANT x2    

@LISTING = % 0.538469310105683 FCONSTANT x1    

@LISTING = % 0.568888888888889 FCONSTANT w0    

@LISTING = % 0.478628670499366 FCONSTANT w1    

@LISTING = % 0.236926885056189 FCONSTANT w2    

@LISTING = : scale    ( :: A B <197> [A+B]/2  [B-A]/2 )         

@LISTING = 	FOVER      F-    F2/    FUNDER F+  ;    

@LISTING = : rescale  ( :: a b x <197> a+b*x)   F*  F+  ;    

@LISTING = : F3R+  ( :: a b c x <197> a+x b c)  F3R  F+  F-ROT ;    

: }integral  ( :: A B <197>)           scale                         ( 
:: <197> [A+B]/2  [B-A]/2 )         FOVER  F(X)  w0 F*  F-ROT     ( 
:: <197> I a b)         XDUP   x1 rescale   F(X)   w1  F*   F3R+         XDUP   x1 
FNEGATE rescale   F(X)   w1  F*   F3R+         XDUP   x2 rescale   F(X)   w2  F*   F3R+         XDUP   x2 
FNEGATE rescale   F(X)   w2  F*   F3R+         FPLUCK  F* ; 

