% https://open.kattis.com/problems/estimatingtheareaofacircle
% 1s
% 1.4 Easy

:- use_module(library(dcg/basics)).
main :-
    once(phrase_from_stream(p(L),user_input)),
    t(L).
main.

t([]) :- writeln(empty).
t([0,0,0]):- writeln(end).
t([[R,M,C]|Ls]) :-
    R > 0,
    E is R*R*pi,
    S is R*R*C*4/M,
    format('~8f ~8f~n',[E,S]),
    t(Ls).

q([R,M,C]) --> (float(R) ; integer(R)), " ",integer(M)," ",integer(C).
q([]) --> [].

p([L|Ls]) --> q(L), {L \= []},eol,p(Ls).
p([L]) --> q(L), eol.
p([])     --> [].



    