% https://open.kattis.com/problems/skocimis
% 1s
% 1.6 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[A,B,C],Ss),
    D1 is B-A, D2 is C-B,
    max_list([D1,D2],Max),
    Max1 is Max-1,
    writeln(Max1).
main.

