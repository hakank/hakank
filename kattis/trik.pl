% https://open.kattis.com/problems/trik
% Time limit: 1s
% Diff: 1.5 Easy

% Submitted a shorter version switch/3 -> s/3 (270 char)
main :-
    read_line_to_string(user_input,S),
    atom_chars(S,Ss),
    Init = [1,2,3],
    switch(Ss,Init,Res),
    nth1(Pos,Res,1),
    writeln(Pos).
main.

switch([],S,S).
switch([C|Cs],S0,S) :-
    apply(C,[S0,S1]),
    switch(Cs,S1,S).

% switch A-B
'A'([A,B,C],[B,A,C]).

% Swith B-C
'B'([A,B,C],[A,C,B]).

% Switch A-C
'C'([A,B,C],[C,B,A]).