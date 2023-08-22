% https://open.kattis.com/problems/forcedchoice
% 1s
% 1.4 Easy


% Time Limit Exceeded for test 9/25

:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p([P,L]),user_input),
    t(L,P).
main.

t([L|Ls],P) :-
    [_|R] = L,
    (memberchk(P,R) -> T = keep ; T = remove),
    writeln(T),
    t(Ls,P).

q([Ns|Ls]) --> string(S), {S \= "",split_string(S," ","",Ss),maplist(number_string,Ns,Ss)}, eol, q(Ls).
q([])     --> [].

p([P,L]) --> integer(_)," ", integer(P), " ", integer(_), eol, q(L).
    