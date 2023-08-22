% https://open.kattis.com/problems/sumkindofproblem
% 1s
% 1.6 Easy

% Using closed forms for the calculations...
% Yes, that worked!

main :-
    cl([_|L]),
    s(L).
main.

s([]).
s([[I,N]|Ls]) :-
    A is (N*(N+1))/2,
    O is N*N,
    E is N*(N+1),
    format('~d ~d ~d ~d~n', [I,A,O,E]),
    s(Ls).

cl(Lines) :-
    rs(X),
    cl(X,[],Lines).
cl(X,Lines,Lines) :-
    X == end_of_file, !.
cl(X,Lines0,[N|Lines]) :-
    split_string(X," ","",Xs),
    maplist(number_string,N,Xs),
    rs(X2),
    cl(X2,Lines0,Lines).
rs(S) :-
    read_line_to_string(user_input,S).
