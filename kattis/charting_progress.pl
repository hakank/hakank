% https://open.kattis.com/problems/chartingprogress
% 1s
% 2.0 Easy

% split_string/3 does not split properly on "\n"\n" (it splits on all "\n"'s),
% so I use DCG instead

:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
main :-
    phrase_from_stream(p(L),user_input),
    s(L).

s([]).
s([S|Ss]) :-
    split_string(S,"\n","\n",T),
    maplist(string_chars,T,Cs),
    transpose(Cs,CsT),
    sort(0,@>=,CsT,SS),
    transpose(SS,ST),
    maplist(w,ST),
    nl,
    s(Ss).

w(S) :- format("~s~n",[S]).

p([L|Ls]) --> string(L),[0'\n,0'\n],p(Ls).
p([L]) --> string(L).
p([]) --> [].