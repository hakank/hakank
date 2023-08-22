% https://open.kattis.com/problems/jumbojavelin
% 1s
% 1.4 Easy

:- use_module(library(dcg/basics)).
main :- phrase_from_stream(p(L),user_input),sum_list(L,Sum),length(L,Len),R is Sum - Len + 1,format('~d~n',[R]).
main.
q([L|Ls]) --> integer(L), eol, q(Ls).
q([]) --> [].
p(L) --> integer(_), eol, q(L).
