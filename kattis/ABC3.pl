% https://open.kattis.com/problems/abc
% 1s
% 2.3 Easy

% This is only some tests with ChatGPT3.5. No new stuff.
% The shortest program is in ANC2.pl

/*
  I asked ChatGPT3.5 to refactor the code in ABC.pl
  """
  Please refactor this Prolog code to make it neater and smaller.
  """

  But it doesn't output anything since P is empty.

*/

% First try of GPT3.5
/*
main :-
readln([A,B,C,L0], end_of_file),
string_chars(L0, L),
writeln(l=L),
findall(J, (member(V, L), nth1(J, [A,B,C], V)), P),
writeln(p=P),
sort(P, R),
writeln(r=R),
maplist(write, R),
nl.
*/

% Second try: R is still empty.
/*
main :-
readln([A,B,C,L0], end_of_file),
string_chars(L0, L),
sort([A,B,C], S),
findall(V, (member(V, S), member(V, L)), R),
writeln(r=R),
maplist(write, R),
nl.
*/

% Third try of GPT3.5. R is still empty.
/*
main :-
    readln([A,B,C,L0], end_of_file),
    string_chars(L0, L),
    sort([A,B,C], S),
    findall(V, (member(V, S), member(V, L)), R),
    writeln(R),
    maplist(write, R),
    nl.
*/

% I try GPT4 instead.
% This works -  what I can see - but it too verbose.
%
/*
main :-
    readln([A, B, C, L0], end_of_file),
    string_chars(L0, L),
    msort(L, Ls),
    find_permuted_indices(L, Ls, P),
    msort([A, B, C], S),
    apply_permutation(S, P, R),
    write_list(R),
    nl.

write_list([]).
write_list([H|T]) :-
    format("~w ", [H]),
    write_list(T).

find_permuted_indices(A, B, P) :-
    length(A, N),
    findall(J, (between(1, N, I), nth1(I, A, V), nth1(J, B, V)), P).

apply_permutation(A, P, B) :-
    length(A, N),
    findall(V, (between(1, N, I), nth1(I, P, J), nth1(J, A, V)), B).
*/

% I asked for a shorter code (and noted that I don't cared about readability)
% GPT4 generated this code instead, But's not correct. it printed 3.
/*
main:-readln([A,B,C,L0],end_of_file),string_chars(L0,L),msort(L,Ls),p(L,P,Ls),msort([A,B,C],S),p(S,R,P),w(R),nl. % ORIG
w([]).
w([H|T]):-format("~w ",[H]),w(T).
p(A,P,B):-length(A,N),findall(J,(between(1,N,I),nth1(I,A,V),nth1(J,B,V)),P).
*/

% I then gave the example
%% Given this data file 
%% 6 4 2
%% CAB
%% the program should output the following:
%% 6 2 4
% and then it generated this, which still outputs just 3,
% P is [3,1,2]
/*
main:-readln([A,B,C,L0],end_of_file),string_chars(L0,L),msort(L,Ls),p(L,P,Ls),msort([A,B,C],S),p(S,R,P),writeln(P),w(R),nl.
w([]).
w([H|T]):-format("~w ",[H]),w(T).
p(A,P,B):-length(A,N),findall(J,(between(1,N,I),nth1(I,A,V),nth1(J,B,V)),P),!.
*/

% Next try. Still just 3
/*
main:-readln([A,B,C,L0],end_of_file),string_chars(L0,L),msort(L,Ls),p(L,P,Ls),msort([A,B,C],S),p(S,R,P),w(R),nl.
w([]).
w([H|T]):-format("~w ",[H]),w(T).
p(A,P,B):-length(A,N),findall(J,(between(1,N,I),nth1(I,A,V),nth1(J,B,V)),P),!.
p(A,P,B):-length(A,N),findall(V,(between(1,N,I),nth1(I,P,J),nth1(J,A,V)),B).
*/

% And a next try. Nope, still just 3. Well, I'm a little less impressed...
main:-readln([A,B,C,L0],end_of_file),string_chars(L0,L),msort(L,Ls),p(L,P,Ls),msort([A,B,C],S),p(S,R,P),w(R),nl.
w([]).
w([H|T]):-format("~w ",[H]),w(T).
p(A,P,B):-length(A,N),findall(J,(between(1,N,I),nth1(I,A,V),nth1(J,B,V)),P).
p(A,P,B):-length(A,N),findall(V,(between(1,N,I),nth1(I,P,J),nth1(J,A,V)),B),!.

/*
% A silly way of doing it shorter...

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

p1(A,P,B) :- l(A,N),f(J,(b(1,N,I),n(I,A,V),n(J,B,V)),P).
p2(A,P,B) :- l(A,N),f(V,(b(1,N,I),n(I,P,J),n(J,A,V)),B).

l(A,B) :- length(A,B).
f(A,B,C) :- findall(A,B,C).
b(A,B,C) :- between(A,B,C).
n(A,B,C) :- nth1(A,B,C).

% p1(A,P,B) :- length(A,N),findall(J,(between(1,N,I),nth1(I,A,V),nth1(J,B,V)),P).
% p2(A,P,B) :- length(A,N),findall(V,(between(1,N,I),nth1(I,P,J),nth1(J,A,V)),B).
*/