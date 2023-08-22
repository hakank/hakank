% https://open.kattis.com/problems/hangman
% 1s
% 1.6 Easy

main :-
    rc(W),
    rc(A),
    (s(A,W,0,_)->T="WIN";T="LOSE"),writeln(T).
main.
s(_,[],M,M).
s([A|As],Cs,M0,M) :-
    M0 < 10,
    (memberchk(A,Cs) ->
        delete(Cs,A,Cs2),
        M1 = M0
    ;
        M1 is M0 + 1,
        Cs2=Cs),
    s(As,Cs2,M1,M).
rc(S):-read_line_to_codes(user_input,S).