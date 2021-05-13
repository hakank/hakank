%% background knowledge

% Prolog's succ/2

succ(A,B) :-
        integer(A),
        B is A - 1.