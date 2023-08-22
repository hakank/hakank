% https://open.kattis.com/problems/kornislav
% Time 1s
% Diff 1.5 Easy

% TODO!
:- use_module(library(clpfd)).
main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,Ns,Ss),
    writeln(Ns),
    findall(Size,move(Ns,Size),Sizes),
    writeln(Sizes),
    nl.
main.

% THIS IS NOT CORRECT!
move(Ns, Size) :-
    permutation(Ns,[A,B,C,D]),
    Size is (A+B) * (C+D).
    