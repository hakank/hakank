% https://open.kattis.com/problems/sifferprodukt
% 1s
% 1.6 Easy

main :-
    read_line_to_codes(user_input,S),
    s(S,P),
    writeln(P).
main.

s([S],T) :- T is S-48, T =< 9.
s(N,P) :-
    p(N,1,N1),
    number_codes(N1,N2),
    s(N2,P).

p([],P,P).
p([N|Ns],P0,P) :-
    I is N-0'0,
    (I > 0 ->
        P1 is P0 * I
    ;
        P1 is P0
    ),
    p(Ns,P1,P).
    