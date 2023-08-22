% https://open.kattis.com/problems/sumsquareddigits
% 1s
% 1.5 Easy

% This is 769 chars (676 if compressed)
% Note: Andriy Zyevakov's solution is 420 chars

/*
   Here's a one-liner in Picat:

 Picat> B=16,V=987654321,X=[parse_radix_string(C.to_string,B)**2 : C in V.to_radix_string(B)].sum          
B = 16
V = 987654321
X = 696

  Se sum_squared_digits.pi

  SWI-Prolog is _slightly_ more verbose than Picat... :-(

*/


:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p(L), user_input),
    c(L).
main.

c([]).
c([[I,B,V]|Ls]) :-
    number_string(B,Bs),
    string_concat(Bs,"r",BsR),
    string_concat("~",BsR,F),
    format(string(X),F,V),
    atom_chars(X,Xs),
    findall(D2,(member(C,Xs),conv(C,D),D2 is D*D),Ds),
    sum_list(Ds,Sum),
    format('~d ~d~n',[I,Sum]),
    c(Ls).

conv('0',0).
conv('1',1).
conv('2',2).
conv('3',3).
conv('4',4).
conv('5',5).
conv('6',6).
conv('7',7).
conv('8',8).
conv('9',9).
conv('a',10).
conv('b',11).
conv('c',12).
conv('d',13).
conv('e',14).
conv('f',15).


q1([I,B,V]) --> integer(I)," ",integer(B), " ",integer(V).

q([L|Ls]) --> q1(L),eol,q(Ls).
q(L) --> q1(L).
q([]) --> [].

p(L) --> integer(_), eol, q(L).
p([]) --> [].

