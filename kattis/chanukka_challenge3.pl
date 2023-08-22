% https://open.kattis.com/problems/chanukah
% 1s
% 1.4 Easy

% Shorter version: 283 chars
% And now it's accepted.

main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss).

s([]).
s([S|Ss]) :-
    split_string(S," ","",T),
    maplist(number_string,[I,N],T),
    X is (N*(N+1) div 2)+N,
    format('~d ~d~n',[I,X]),
    s(Ss).
