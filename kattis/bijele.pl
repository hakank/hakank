% https://open.kattis.com/problems/bijele
% CPU Time limit: 1s
% Difficulty: 1.4 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S," ", "", Ss),
    maplist(number_string,Ns,Ss),
    L=[1,1,2,2,2,8],
    maplist(diff,L,Ns,Ds),
    maplist(format('~d '),Ds),
    nl.
main.

diff(Wanted,Given,Diff) :-
    Diff is Wanted-Given.
