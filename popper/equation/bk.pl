plus(X,Y,Z) :-
        integer(X),
        integer(Y),
        Z is X + Y.

minus(X,Y,Z) :-
        integer(X),
        integer(Y),
        Z is X - Y.

mult(X,Y,Z) :-
        integer(X),
        integer(Y),
        Z is X * Y.

div(X,Y,Z) :-
        integer(X),
        integer(Y),
        Y > 0,
        Z is X / Y.
