% https://open.kattis.com/problems/abc
% 1s
% 2.3 Easy

% Accepted, but it should be much shorter...
% I first tried to make the "permutation lookup" (p1 and p2) as a reversible
% predicate, but didn't succeed with that...

% Fixes: using readln/2 instead of read_string/3

main :-
    readln([A,B,C,L0],end_of_file),
    string_chars(L0,L),
    msort(L,Ls),
    p1(L,P,Ls),
    msort([A,B,C],S),
    p2(S,P,R),
    w(R),
    nl.
w([]).
w([H|T]) :- format("~w ",[H]), w(T).    
p1(A,P,B) :- length(A,N),findall(J,(between(1,N,I),nth1(I,A,V),nth1(J,B,V)),P).
p2(A,P,B) :- length(A,N),findall(V,(between(1,N,I),nth1(I,P,J),nth1(J,A,V)),B).

