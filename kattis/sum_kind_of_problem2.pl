% https://open.kattis.com/problems/sumkindofproblem
% 1s
% 1.6 Easy

% Replacing the DCG with plain reading.
% Is this faster? Nope, still Time Limit! Sigh!

main :-
    cl([_|L]),
    s(L).
main.

s([]).
s([[I,N]|Ls]) :-
    s(1,N,0,A,0,O,0,E),
    format('~d ~d ~d ~d~n', [I,A,O,E]),
    s(Ls).

s(N1,N,A,A,O,O,E,E) :- N1 > N, !.
s(I,N,A0,A,O0,O,E0,E) :-
    A1 is A0+I,
    O1 is O0+(2*I-1),
    E1 is E0+(2*I),    
    I2 is I+1,
    s(I2,N,A1,A,O1,O,E1,E).

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
