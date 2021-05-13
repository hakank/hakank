%% background knowledge

s(10,9).
s(9,8).
s(8,7).
s(7,6).
s(6,5).
s(5,4).
s(4,3).
s(3,2).
s(2,1).
s(1,0).


% Testing
odd(X) :-
        integer(X),
        1 is X mod 2.


succ(X,Y) :-
        integer(X),
        integer(Y),
        Y is X + 1.

eq(X,Y).
zero(0).
one(1).