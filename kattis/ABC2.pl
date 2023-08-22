% https://open.kattis.com/problems/abc
% 1s
% 2.3 Easy

% Accepted, but it should be much shorter...
% I first tried to make the "permutation lookup" (p1 and p2) as a reversible
% predicate, but didn't succeed with that...

% Fixes:
%  - using readln/2 instead of read_string/3
%  - using member/2 instead of length/2 + between/3

% 328 chars
main :-
    readln([A,B,C,L0],end_of_file),
    string_chars(L0,L),
    msort(L,Ls),
    msort([A,B,C],S),
    same_length(L,Ls),
    p1(L,P,Ls),
    
    p2(S,P,R),
    writeln([s=S,p=P,r=R]),
    w(R),
    nl.
w([]).
w([H|T]) :- format("~w ",[H]), w(T).
p1(A,P,B) :- findall(I,(member(V,A),nth1(I,B,V)),P).
p2(A,P,B) :- findall(V,(member(I,P),nth1(I,A,V)),B).

/* 
% Compressed: 278 chars. Still way too long for Top 10 (61..73 chars)
main:-readln([A,B,C,L0],end_of_file),string_chars(L0,L),msort(L,Ls),msort([A,B,C],S),same_length(L,Ls),p1(L,P,Ls),p2(S,P,R),w(R),nl.
w([]). w([H|T]) :- format("~w ",[H]), w(T).
p1(A,P,B):-findall(I,(member(V,A),nth1(I,B,V)),P).
p2(A,P,B):-findall(V,(member(I,P),nth1(I,A,V)),B).
*/