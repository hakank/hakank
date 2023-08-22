% https://open.kattis.com/problems/tarifa
% 1s
% 1.4 Easy

main :-
    read_int(Max), read_int(Num),
    read_all_int(Ns),
    sum_list(Ns,Sum),
    X is Max*Num - Sum + Max,
    writeln(X).
main.


read_all_int(Ns) :-
    read_string(In),
    read_all_string(In,[],S),
    maplist(number_string,Ns,S).
read_all_string(end_of_file,S,S).
read_all_string(In,S0,[In|S]) :-
    read_string(S2),
    read_all_string(S2,S0,S).

read_int(N) :-
    read_string(In),
    number_string(N,In).

read_string(S) :-
    read_line_to_string(user_input,S).
