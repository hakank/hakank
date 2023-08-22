% https://open.kattis.com/problems/numberfun
% 1s
% 1.5 Easy

:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p(L),user_input),
    t(L).
main.


t([]).
t([[A,B,C]|T]) :-
    (c(A,B,C) -> X = "Possible" ; X = "Impossible"),
    writeln(X),
    t(T).

c(A,B,C) :- C is A*B.
c(A,B,C) :- C is A+B.
c(A,B,C) :- C is A-B ; C is B-A.
c(A,B,C) :- A is B*C ; B is A*C.

q1([A,B,C])  --> integer(A)," ", integer(B), " ", integer(C), eol.
q1([])  --> [].

q([L|Ls]) --> q1(L),{L \= []},q(Ls).
q([]) --> [].


p(L) --> integer(_), eol, q(L).
