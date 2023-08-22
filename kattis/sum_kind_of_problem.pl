% https://open.kattis.com/problems/sumkindofproblem
% 1s
% 1.6 Easy


% Time Limit Exeeded on test 2/2.
% I'm not sure how to speed this up...
% In sum_kind_of_problem2.pl I replace DCG with read_line_to_string/2
% but it's still too slow.
% Ah, but there are closed forms for this! Doing that in sum_kind_of_problem3.pl
% which is accepted.

:- use_module(library(dcg/basics)).
main :-
    once(phrase_from_stream(p(L),user_input)),
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

q1([A,B]) --> integer(A)," ", integer(B).

q([L|Ls]) --> q1(L), eol, q(Ls).
q([L]) --> q1(L).
q([]) --> [].

p(L) --> integer(_),eol, q(L).
