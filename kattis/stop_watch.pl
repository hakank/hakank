% https://open.kattis.com/problems/stopwatch
% 1s
% 1.5 Easy

:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p([N,L]),user_input),
    (N mod 2 =:= 1 ->
        T = "still running"
    ;
        w(L,0,T)
    ),
    writeln(T).
main.

w([],S,S).
w([T1,T2|Ts],S0,S) :-
    S1 is S0+T2-T1,
    w(Ts,S1,S).

q([L|Ls]) --> integer(L),eol,q(Ls).
q([]) --> [].
p([N,L]) --> integer(N),eol,q(L).
    