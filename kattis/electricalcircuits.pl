% https://open.kattis.com/problems/electricaloutlets
% Time limit: 1s
% Diff 1.3 Easy

main :-
    read_string(_),
    repeat,
    read_string(S),
    (S == end_of_file ;
        split_string(S," ", "",Ss),
        maplist(number_string,Ns,Ss),
        check_circuit(Ns, Num),
        writeln(Num),
        fail
    ).
main.

check_circuit(Ns, Num) :-
    sum_list(Ns,Sum),
    length(Ns,Len),
    Num is Sum - 2*(Len-2) - 2*1 + 1.

read_string(S) :-
    read_line_to_string(user_input,S).
