% https://open.kattis.com/problems/knotknowledge
% 1s
% 1.4 Easy

main :-
    readln([N|S],end_of_file),
    length(L,N),
    append(L,K,S),
    member(Q,L),
    not(memberchk(Q,K)),
    writeln(Q).

/*
% Compressed: 101 chars
main:-readln([N|S],end_of_file),length(L,N),append(L,K,S),member(Q,L),not(memberchk(Q,K)),writeln(Q).

*/

/*
% Older version: 152 chars
main:-r(_),s(L),s(K),member(Q,L),not(memberchk(Q,K)),writeln(Q).
main.
s(Ss):-r(S),split_string(S," ", "", Ss).
r(S):-read_line_to_string(user_input,S).

*/