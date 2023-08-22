% https://open.kattis.com/problems/npuzzle
% 1s
% 2.1 Easy

% Note: This is not about solving the puzzle
% (unfortunately or fortunately depending how you look at it),
% just calculating the Manhattan distance from the init positions.
% The matrix is represented as
%    0  1  2  3
%    4  5  6  7
%    8  9 10 11
%   12 13 14 15
% to simplify with div and mod.

% See also
%  - npuzzle.pi
%  - npuzzle.py: Plance 2 in Top 10 shortest programs (193 chars)
%
main :-
    read_string(user_input,_,S),
    string_codes(S,C0),
    delete(C0,10,C),
    s(C,0,0,R),
    writeln(R).
s([],_,R,R).
s([C|Cs],I,R0,R) :-
    (C \= 0'. ->
        V is C - 0'A,
        d(I,V,D),
        R1 is R0+D
    ;
        R1 is R0
    ),
    I1 is I+1,
    s(Cs,I1,R1,R).
p(A,R,C) :- R is A div 4, C is A mod 4.
d(A,B,D) :-
    p(A,AR,AC),
    p(B,BR,BC),
    D is abs(AR-BR)+abs(AC-BC).


/*
% Compressed 295 chars. Not short enough from Top 10 (151..223) 
main:-read_string(user_input,_,S),string_codes(S,C0),delete(C0,10,C),s(C,0,0,R),writeln(R).
s([],_,R,R). s([C|Cs],I,R0,R):-(C\=0'. ->V is C-0'A,d(I,V,D),R1 is R0+D;R1 is R0),I1 is I+1,s(Cs,I1,R1,R).
p(A,R,C):-R is A div 4,C is A mod 4.
d(A,B,D):-p(A,AR,AC),p(B,BR,BC),D is abs(AR-BR)+abs(AC-BC).
*/