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

pow(X,Y,Z) :-
        integer(X),
        integer(Y),
        Z is X ** Y.


const1(1).
const2(2).
const3(3).
const4(4).