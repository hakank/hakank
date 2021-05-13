%% background knowledge

plus(X,Y,Z) :- nonvar(X), nonvar(Y), Z is X+Y.
minus(X,Y,Z) :- nonvar(X), nonvar(Y), Z is X-Y.
division(X,Y,Z) :- nonvar(X), nonvar(Y), Y > 0, Z is X / Y.
times(X,Y,Z) :- nonvar(X), nonvar(Y), Z is X*Y.
