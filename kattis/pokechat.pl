% https://open.kattis.com/problems/pokechat
% 1s
% 1.4 Easy

:- use_module(library(dcg/basics)).
main :-
    read_code(S),read_code(C),chunks(Ns,C,[]),
    findall(X,(member(N,Ns),nth1(N,S,X)),Xs),
    format('~s~n',[Xs]).
main.
read_code(S) :- read_line_to_codes(user_input,S).
chunks([N|Cs]) --> [A,B,C],{number_codes(N,[A,B,C])},chunks(Cs).
chunks([]) --> [].
