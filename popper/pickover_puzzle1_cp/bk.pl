:- use_module(library(clpfd)).
plus(X,Y,Z) :-
        [X,Y,Z] ins 0..100,
        % integer(X),
        % integer(Y),
        Z #= X + Y.

minus(X,Y,Z) :-
        [X,Y,Z] ins 0..100,                
        %% integer(X),
        %% integer(Y),
        Z #= X - Y.

mult(X,Y,Z) :-
        [X,Y,Z] ins 0..100,        
        %% integer(X),
        %% integer(Y),
        Z #= X * Y.

div(X,Y,Z) :-
        [X,Y,Z] ins 0..100,        
        %% integer(X),
        %% integer(Y),
        Y #> 0,
        Z #= X / Y.


/*
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
*/
