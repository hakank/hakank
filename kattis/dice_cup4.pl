% https://open.kattis.com/problems/dicecup
% 1s
% 1.4 Easy

% Trying to beat Andriy Zyevakov's 176 chars solution.
% Another idea: The range of the most common values is LowDie+1 .. HighDie+1.
% It works and it's shorter: 165 chars (compressed and with bad naming, but still)


main:-read_line_to_codes(user_input,S),append(X,[32|Y],S),maplist(number_string,[C,D],[X,Y]),msort([C,D],[E,F]),A is E+1,B is F+1,numlist(A,B,L),maplist(writeln,L).


% Testing the new approach:
main_test:-
    % read_line_to_codes(user_input,S),append(AS,[32|BS],S),
    % maplist(number_string,[A,B],[AS,BS]),
    between(4,20,A),between(4,20,B),
    writeln([a=A,b=B]),
    s(A,B,L1), % new approach
    writeln(L1),
    s_old(A,B,L2), % the old one
    writeln(L2),
    (L1 \= L2 ->
        writeln(not_the_same),
        halt
    ;
        true
    ),
    nl,
    fail.
main_test.

% The new approach
s(A0,B0,L) :-
    msort([A0,B0],[A,B]),
    A1 is A+1,B1 is B+1,
    numlist(A1,B1,L).
    

% This is my original approach
s_old(A,B,L) :-     
     findall(Sm,(b(1,A,I),b(1,B,J),Sm is I+J),C),
     msort(C,Cs),
     clumped(Cs,Cl),
     sort(2,@>=,Cl,Srt),[_-Max|_]=Srt,
     findall(K,(member(K-Max,Srt)),Ks),
     sort(Ks,L).

b(L,U,I):-between(L,U,I).
