predicates
   bror(symbol, symbol)
   gud(symbol)
   far(symbol, symbol)
   son(symbol)
   asagud(symbol)
   farfar(symbol)
   gullig(symbol)
   
clauses   

bror( Y , Z ) :- far( X , Y ), far( X , Z )
( balder , oden ).
far( oden , tor ).
far( nisse , hakan ).
far(hakan , kurt ).
far( hakan , olle ).
son ( Y ) :- far( X , Y).
asagud( oden ).
asagud( tor ).
asagud( balder ).
asagud( hakan).
asagud( nisse ).
asagud( olle ).
asagud( kurt ).
farfar( Z ):- far( X ,Y ), far( Z , X ).
gullig( hakan ).
gullig( balder ).































