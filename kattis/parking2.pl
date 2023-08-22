% https://open.kattis.com/problems/parking2
% Time limit: 1s
% Diff: 1.5 Easy

main :-
    read_string(_),
    repeat,
    read_string(S),
    (S == end_of_file ;
        read_string(S2),
        split_string(S2," ","", Ss),
        maplist(number_string,Ns,Ss),
        min_list(Ns,Min),
        max_list(Ns,Max),
        Res is (Max-Min)*2,
        writeln(Res),
        fail
    ).
main.

read_string(S) :-
    read_line_to_string(user_input,S).