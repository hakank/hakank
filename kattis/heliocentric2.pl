% https://open.kattis.com/problems/heliocentric
% 1s
% 1.7 Easy

% Earth: 365 days orbit
% Mars:  687 days

% Using clpfd. Shorter and slower (0.26s vs 0.06s).
% And I'm not sure about the upper bound of T,
% so I set it to lcm(365,687) = 250755

:- use_module(library(clpfd)).
main :-
    read_string(user_input,10000,S),
    split_string(S,"\n","\n",Ss),
    s(1,Ss).
main.

s(_,[]).
s(I,[L|Ls]) :-
    split_string(L," ","",Ss),
    maplist(number_string,[E,M],Ss),
    T in 0..250755,
    (E+T) mod 365 #= 0,
    (M+T) mod 687 #= 0,
    labeling([min(T)],[T]),
    format('Case ~d: ~d~n',[I,T]),
    I1 is I+1,
    s(I1,Ls).
