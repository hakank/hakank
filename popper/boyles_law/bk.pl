precision(0.01).

plus(X,Y,Z) :-
        nonvar(X),
        nonvar(Y),
        Z is X + Y.

approx_plus(X,Y,Z) :-
        nonvar(X),
        nonvar(Y),
        nonvar(Z),
        precision(Prec),
        T is X+Y,
        Z >= T - Prec,
        Z =< T + Prec.


minus(X,Y,Z) :-
        nonvar(X),
        nonvar(Y),
        Z is X - Y.

approx_minus(X,Y,Z) :-
        nonvar(X),
        nonvar(Y),
        nonvar(Z),
        precision(Prec),        
        T is X+Y,
        Z >= T - Prec,
        Z =< T + Prec.


mult(X,Y,Z) :-
        nonvar(X),
        nonvar(Y),
        Z is X * Y.

approx_mult(X,Y,Z) :-
        nonvar(X),
        nonvar(Y),
        nonvar(Z),
        precision(Prec),        
        T is X*Y,
        Z >= T - Prec,
        Z =< T + Prec.

div(X,Y,Z) :-
        nonvar(X),
        nonvar(Y),
        Y > 0,
        Z is X / Y.


approx_div(X,Y,Z) :-
        nonvar(X),
        nonvar(Y),
        nonvar(Z),
        Y > 0,
        precision(Prec),         
        T is X / Y,
        Z >= T - Prec,
        Z =< T + Prec.



