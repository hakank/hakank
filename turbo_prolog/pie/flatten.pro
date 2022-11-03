flatten(Xs, Ys)  :-
      (Xs, [], Ys).
flatten([X|Xs],Zs,Ys) :- 
      flatten(Xs, Zs, Ys1), flatten(X,Ys, Ys).
      
flatten(X, Xs[X|Xs]) :-
      nonvar(X), atom(X).

flatten([], Xs, Xs).            
       