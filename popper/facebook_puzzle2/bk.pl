:- use_module(library(clpfd)).

% plus(X,Y,Z) :-
%         integer(X),
%         integer(Y),
%         Z is X + Y.

plus(X,Y,Z) :-
        integer(X),
        integer(Y),
        Z #= X + Y.

plus3(X,Y,Z,A) :-
        % writeln(plus3(X,Y,Z,A)),
        integer(X),
        integer(Y),
        integer(Z),        
        A #= X + Y + Z.


minus(X,Y,Z) :-
        % integer(X),
        % integer(Y),
        Z #= X - Y.

mult(X,Y,Z) :-
        % integer(X),
        % integer(Y),
        Z #= X * Y.

div(X,Y,Z) :-
        % integer(X),
        % integer(Y),
        Y #> 0,
        Z #= X / Y.

eq(X,X).
eq3(X,X,X).

not_eq(X,Y) :- X #\= Y.

nop(X) :- integer(X). %% Testing
nop2(X,Y) :- integer(X),integer(Y). %% Testing
nop3(X,Y,Z) :- integer(X),integer(Y),integer(Z). %% Testing