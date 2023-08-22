% https://open.kattis.com/problems/lastfactorialdigit
% 1s
% 1.5 Easy

% Shorter version
main :-
    read_stream_to_codes(user_input,S),
    [_,_|S2] = S,
    maplist(f,S2).
main.

f(10) :- !.
f(49) :- writeln(1),!.
f(N) :-
    number_codes(N1,[N]),
    numlist(2,N1,Is),
    foldl(mult,Is,1,F),
    number_chars(F,Cs),
    last(Cs,C),
    writeln(C).

mult(A,B,C) :- C is A*B.