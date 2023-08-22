% https://open.kattis.com/problems/skruop
% 1s
% 1.3-1.6 Easy

:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p(L),user_input),
    c(L,7,C),
    writeln(C).
main.

c([],S,S).
c([V|Vs],S0,S) :-
    T1 is S0 + V,
    (T1 > 10 -> T = 10 ;
        (T1 < 0 -> T = 0 ; T = T1 )
    ),
    c(Vs,T,S).
        

q1(L) --> string(S), {S \= [], (S == [83,107,114,117,32,111,112,33] -> L = 1 ; L = -1)}.

q([L|Ls]) --> q1(L), eol, q(Ls).
q([L]) --> q1(L).
q([]) --> [].

p(L) --> integer(_), eol, q(L).
    