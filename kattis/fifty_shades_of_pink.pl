% https://open.kattis.com/problems/fiftyshades
% 1s
% 1.5 Easy

:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p(L),user_input),
    maplist(string_codes,S,L),
    findall(W,(member(W,S),re_match("pink|rose"/i,W)),All),
    length(All,Len),
    (Len =:= 0 -> T = "I must watch Star Wars with my daughter" ; T = Len),
    writeln(T).
main.

q([L|Ls]) --> string(L), {L \= []}, eol, q(Ls).
q([L])    --> string(L).
q([])     --> [].
p(L)      --> integer(_), eol, q(L).