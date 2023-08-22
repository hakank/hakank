% https://open.kattis.com/problems/heartrate
% 1s
% 1.4 Easy


:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p(L),user_input),
    maplist(f,L).
main.

f([B,P]) :-
    Min is 60*(B-1)/P,
    CalcBPM is 60*B/P,
    Max is 60*(B+1)/P,
    format('~6f ~6f ~6f~n',[Min,CalcBPM,Max]).

q1([B,P]) --> integer(B)," ", float(P).

q([L|Ls]) --> q1(L), eol, q(Ls).
q([L]) --> q1(L).
q([]) --> [].

p(L) --> integer(_), eol, q(L).