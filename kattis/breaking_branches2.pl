% https://open.kattis.com/problems/breakingbranches
% 1s
% 1.6 Easy

% Trying to make it shorter: 69 chars (vs 157 chars in breaking_branches.pl)

% 
main:-readln([N]),(N mod 2=:=0->format("Alice~n1~n");writeln("Bob")).


/*
% Uncompressed: 96 chars
main :- 
    readln([N]),
    (N mod 2=:=0->format("Alice~n1~n")
    ;
    writeln("Bob")
    ).
*/