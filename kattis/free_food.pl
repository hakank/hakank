% https://open.kattis.com/problems/freefood
% 1s
% 1.5 Easy

:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p(L),user_input),
    append(L,L2),
    sort(L2,Sort),
    length(Sort,Len),
    writeln(Len).
main.
q1(Int) --> integer(A)," ",integer(B), {numlist(A,B,Int)}.
q([L|Ls]) --> q1(L),eol,q(Ls).
q([L])      --> q1(L).
q([])     --> [].
p(L) --> integer(_), eol, q(L).
