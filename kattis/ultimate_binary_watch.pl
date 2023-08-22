% https://open.kattis.com/problems/ultimatebinarywatch
% 1s
% 1.6 Easy

% 123456789
% . *   . .
% . .   . .
% . .   * .
% . .   * .

:- use_module(library(clpfd)).
main :-
    read_line_to_codes(user_input,S),
    c(S,[],Res),
    transpose(Res,ResT),
    maplist(p,ResT).
main.

p([A,B,C,D]) :-
    format('~c ~c   ~c ~c~n',[A,B,C,D]).

c([],S,S).
c([T|Ts],S0,[C|S]) :-
    N is T-0'0,
    b(N,B),
    atom_codes(B,C),
    c(Ts,S0,S).

b(0,'....').
b(1,'...*').
b(2,'..*.').
b(3,'..**').
b(4,'.*..').
b(5,'.*.*').
b(6,'.**.').
b(7,'.***').
b(8,'*...').
b(9,'*..*').

