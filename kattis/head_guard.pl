% https://open.kattis.com/problems/headguard
% 1s
% 2.1 Easy

% Uncompressed: 167 chars -> Place 7 in Top 10
% Problems where I can use clumped/2 tends to be
% quite short compared to other programming languages.

main :-
    readln(S,end_of_file),
    s(S).
s([]).
s([S|T]) :-
    string_chars(S,C),
    clumped(C,L),
    maplist(p,L),nl,
    s(T).
p(A-C) :- format("~d~w",[C,A]).

/*
% Compressed: 133 chars -> Place 3 in Top 10
main:-readln(S,end_of_file),s(S).
s([]). s([S|T]):-string_chars(S,C),clumped(C,L),maplist(p,L),nl,s(T).
p(A-C):-format("~d~w",[C,A]).

*/