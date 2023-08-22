% https://open.kattis.com/problems/detaileddifferences
% 1s
% 1.4 Easy

:- use_module(library(dcg/basics)).
main :- once(phrase_from_stream(p(L),user_input)),check_pairs(L).
main.
q([L|Ls])-->string(L),{L\=[]},eol,q(Ls).
q([]) --> [].
p(L) --> integer(_), eol, q(L).
check_pairs([]).
check_pairs([P1,P2|Rest]) :- format('~s~n~s~n',[P1,P2]), check_pair(P1,P2),nl,nl,check_pairs(Rest).
check_pair([],[]).
check_pair([A|As],[B|Bs]) :-(A==B->T='.';T='*'),write(T),check_pair(As,Bs).
