
domains

    list = symbol*

predicates
   member(symbol, list)


clauses
   member(X, [X|Xs]) if !.
   member(X, [Y|Ys]) if member(X,Ys).

