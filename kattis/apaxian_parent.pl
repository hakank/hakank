% https://open.kattis.com/problems/apaxianparent
% 1s
% 1.6 Easy

% Run time error. Probably regex that's not supported!?
% :- use_module(library(pcre)).
% Ah, regexes are not supported in 8.4.2.
% https://www.swi-prolog.org/download/stable/doc/SWI-Prolog-8.4.2.pdf
% re_matchsub("^([^ ]+?)(.)(.) (.+)$",S,Sub),
% s(Sub.1,Sub.2,Sub.3,Sub.4,R),

% Using DCG works, though a little verboser.

:- use_module(library(dcg/basics)).
main:-read_line_to_codes(user_input,S),s(R,S,[]),flatten(R,Res),format('~s~n',[Res]).
main.
s([A,0'e,0'x,B])-->string(A),[0'e,0'x,0' ],string(B).
s([A,C1,0'e,0'x,B])-->string(A),[C1,C2,0' ],string(B),{memberchk(C2,[0'a,0'i,0'o,0'u])}.
s([A,C1,0'e,0'x,B])-->string(A),[C1,0'e,0' ],string(B).
s([A,C1,C2,0'e,0'x,B])-->string(A),[C1,C2,0' ],string(B).
s([])-->[].
