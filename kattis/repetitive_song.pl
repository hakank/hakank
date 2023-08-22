% https://open.kattis.com/problems/repetitivesong
% 1s
% 2.5 Easy

% TODO!

main :-
    readln([_|S],end_of_file),
    writeln(S),
    clumped(S,C),
    writeln(C).