c(X) :- consult(X).
f(X) :- forget(X).


append( [] , L , L).
append( [X|List1] , List2 , [X|List3] ) :- append( List1, List2,
                                           List3).


consultfile( Filename ) :-  see( Filename ),
                            repeat,
                            doread( X ),
                            assertclause( X ),
                            var( X ),
                            see( Filename ),
                            see( user ).

doread( X ) :- read( X ), !.
doread( _ ).

assertclause( X ) :- assertz ( X ), !.
assertclause( _ ).

rec(File) :- forget(File) , consult(File).

ass(X) :- asserta(X).

list(X) :- listing(X).




