append( [], L , L ).
append( [X|List1], List2, [X|List3] ) :- append( List1, List2,
                                         List3 ).
shopping( X ) :- X = [ eggs , carrots , hamburger , fleacollar ] .
next( X )     :- X = [ tomatoes, onions, ketchup ].
