% https://open.kattis.com/problems/apaxiaaans
% Time limit: 1s
% Diff 1.4 Easy

% Trying to make it simpler/shorter.
% clumped/2 to the rescue! 128char (non compressed.)

main :-
    read_line_to_codes(user_input,S),
    clumped(S,Cl),
    findall(C,member(C-_,Cl),R),
    format('~s~n',[R]).
main.

/*
% Compressed: 100 chars
main:-read_line_to_codes(user_input,S),clumped(S,Cl),findall(C,member(C-_,Cl),R),format('~s~n',[R]).

*/